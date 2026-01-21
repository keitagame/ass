#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <ctype.h>

#define MAX_LINE 256
#define MAX_LABELS 100
#define MAX_CODE 4096
#define MAX_RELOCS 100
#define MAX_SYMBOLS 100

typedef struct {
    char name[64];
    uint64_t addr;
    int global;
} Label;

typedef struct {
    uint64_t offset;
    char symbol[64];
    int type; // 2=R_X86_64_PC32, 10=R_X86_64_32, 1=R_X86_64_64
    int addend;
} Relocation;

typedef struct {
    char name[64];
    uint64_t value;
    int section; // 0=UND, 1=.text
    int binding; // 0=LOCAL, 1=GLOBAL
    int type; // 0=NOTYPE, 2=FUNC
    uint64_t size;
} Symbol;

typedef struct {
    uint8_t code[MAX_CODE];
    size_t size;
    Label labels[MAX_LABELS];
    int label_count;
    Relocation relocs[MAX_RELOCS];
    int reloc_count;
    Symbol symbols[MAX_SYMBOLS];
    int symbol_count;
} Assembler;

// レジスタエンコーディング
int get_reg(const char *reg) {
    if (strcmp(reg, "rax") == 0 || strcmp(reg, "eax") == 0 || strcmp(reg, "al") == 0) return 0;
    if (strcmp(reg, "rcx") == 0 || strcmp(reg, "ecx") == 0 || strcmp(reg, "cl") == 0) return 1;
    if (strcmp(reg, "rdx") == 0 || strcmp(reg, "edx") == 0 || strcmp(reg, "dl") == 0) return 2;
    if (strcmp(reg, "rbx") == 0 || strcmp(reg, "ebx") == 0 || strcmp(reg, "bl") == 0) return 3;
    if (strcmp(reg, "rsp") == 0 || strcmp(reg, "esp") == 0) return 4;
    if (strcmp(reg, "rbp") == 0 || strcmp(reg, "ebp") == 0) return 5;
    if (strcmp(reg, "rsi") == 0 || strcmp(reg, "esi") == 0) return 6;
    if (strcmp(reg, "rdi") == 0 || strcmp(reg, "edi") == 0) return 7;
    if (strcmp(reg, "r8") == 0) return 8;
    if (strcmp(reg, "r9") == 0) return 9;
    if (strcmp(reg, "r10") == 0) return 10;
    if (strcmp(reg, "r11") == 0) return 11;
    if (strcmp(reg, "r12") == 0) return 12;
    if (strcmp(reg, "r13") == 0) return 13;
    if (strcmp(reg, "r14") == 0) return 14;
    if (strcmp(reg, "r15") == 0) return 15;
    return -1;
}

int is_64bit_reg(const char *reg) {
    return reg[0] == 'r' && strlen(reg) <= 3;
}

int is_32bit_reg(const char *reg) {
    return reg[0] == 'e' || (reg[0] == 'r' && isdigit(reg[1]));
}

int find_label(Assembler *as, const char *name) {
    for (int i = 0; i < as->label_count; i++) {
        if (strcmp(as->labels[i].name, name) == 0) {
            return i;
        }
    }
    return -1;
}

void emit(Assembler *as, uint8_t byte) {
    as->code[as->size++] = byte;
}

void emit32(Assembler *as, uint32_t val) {
    emit(as, val & 0xFF);
    emit(as, (val >> 8) & 0xFF);
    emit(as, (val >> 16) & 0xFF);
    emit(as, (val >> 24) & 0xFF);
}

void emit64(Assembler *as, uint64_t val) {
    emit32(as, val & 0xFFFFFFFF);
    emit32(as, (val >> 32) & 0xFFFFFFFF);
}

void add_relocation(Assembler *as, const char *symbol, int type) {
    Relocation *r = &as->relocs[as->reloc_count++];
    strcpy(r->symbol, symbol);
    r->offset = as->size;
    r->type = type;
    r->addend = -4; // PC相対の場合
}

void assemble_mov(Assembler *as, char *dst, char *src) {
    int dst_reg = get_reg(dst);
    int src_reg = get_reg(src);
    
    if (dst_reg >= 0 && src_reg >= 0) {
        if (is_64bit_reg(dst)) {
            emit(as, 0x48 | ((dst_reg >> 3) << 2) | (src_reg >> 3));
        }
        emit(as, 0x89);
        emit(as, 0xC0 | (src_reg << 3) | dst_reg);
    } else if (dst_reg >= 0 && isdigit(src[0])) {
        long long val = strtoll(src, NULL, 0);
        if (is_64bit_reg(dst)) {
            emit(as, 0x48 | (dst_reg >> 3));
            emit(as, 0xB8 + (dst_reg & 7));
            emit64(as, val);
        } else {
            emit(as, 0xB8 + dst_reg);
            emit32(as, val);
        }
    }
}

void assemble_add(Assembler *as, char *dst, char *src) {
    int dst_reg = get_reg(dst);
    int src_reg = get_reg(src);
    
    if (dst_reg >= 0 && src_reg >= 0) {
        if (is_64bit_reg(dst)) {
            emit(as, 0x48);
        }
        emit(as, 0x01);
        emit(as, 0xC0 | (src_reg << 3) | dst_reg);
    } else if (dst_reg >= 0 && isdigit(src[0])) {
        long long val = strtoll(src, NULL, 0);
        if (is_64bit_reg(dst)) {
            emit(as, 0x48);
        }
        if (val >= -128 && val <= 127) {
            emit(as, 0x83);
            emit(as, 0xC0 | dst_reg);
            emit(as, val & 0xFF);
        } else {
            emit(as, 0x81);
            emit(as, 0xC0 | dst_reg);
            emit32(as, val);
        }
    }
}

void assemble_call(Assembler *as, char *target) {
    emit(as, 0xE8); // CALL rel32
    
    int label_idx = find_label(as, target);
    if (label_idx >= 0) {
        // ローカルラベル
        int32_t offset = as->labels[label_idx].addr - (as->size + 4);
        emit32(as, offset);
    } else {
        // 外部シンボル - 再配置情報を追加
        add_relocation(as, target, 2); // R_X86_64_PC32
        emit32(as, 0);
    }
}

void assemble_syscall(Assembler *as) {
    emit(as, 0x0F);
    emit(as, 0x05);
}

void assemble_ret(Assembler *as) {
    emit(as, 0xC3);
}

void parse_line(Assembler *as, char *line, int pass) {
    char *token = strtok(line, " \t\n,");
    if (!token || token[0] == ';') return;
    
    // .globl ディレクティブ
    if (strcmp(token, ".globl") == 0 || strcmp(token, ".global") == 0) {
        char *name = strtok(NULL, " \t\n");
        if (name && pass == 1) {
            int idx = find_label(as, name);
            if (idx >= 0) {
                as->labels[idx].global = 1;
            }
        }
        return;
    }
    
    // ラベル処理
    if (strchr(token, ':')) {
        token[strlen(token) - 1] = '\0';
        if (pass == 1) {
            strcpy(as->labels[as->label_count].name, token);
            as->labels[as->label_count].addr = as->size;
            as->labels[as->label_count].global = 0;
            as->label_count++;
        }
        return;
    }
    
    if (pass == 2) {
        if (strcmp(token, "mov") == 0) {
            char *dst = strtok(NULL, " \t\n,");
            char *src = strtok(NULL, " \t\n,");
            if (dst && src) assemble_mov(as, dst, src);
        } else if (strcmp(token, "add") == 0) {
            char *dst = strtok(NULL, " \t\n,");
            char *src = strtok(NULL, " \t\n,");
            if (dst && src) assemble_add(as, dst, src);
        } else if (strcmp(token, "call") == 0) {
            char *target = strtok(NULL, " \t\n,");
            if (target) assemble_call(as, target);
        } else if (strcmp(token, "syscall") == 0) {
            assemble_syscall(as);
        } else if (strcmp(token, "ret") == 0) {
            assemble_ret(as);
        }
    }
}

void write_string(FILE *f, const char *str) {
    fwrite(str, 1, strlen(str) + 1, f);
}

void write_elf_object(FILE *out, Assembler *as) {
    // ELF64ヘッダ
    uint8_t elf_header[64] = {
        0x7F, 'E', 'L', 'F', 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        1, 0, 0x3E, 0, 1, 0, 0, 0, // ET_REL, EM_X86_64
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0x40, 0, 0, 0, 0, 0, 0x40, 0, 0, 0, 0, 0
    };
    
    // シンボルテーブル構築
    Symbol *null_sym = &as->symbols[as->symbol_count++];
    memset(null_sym, 0, sizeof(Symbol));
    
    Symbol *text_sym = &as->symbols[as->symbol_count++];
    strcpy(text_sym->name, ".text");
    text_sym->section = 1;
    text_sym->type = 3; // STT_SECTION
    
    for (int i = 0; i < as->label_count; i++) {
        Symbol *sym = &as->symbols[as->symbol_count++];
        strcpy(sym->name, as->labels[i].name);
        sym->value = as->labels[i].addr;
        sym->section = 1; // .text
        sym->binding = as->labels[i].global ? 1 : 0;
        sym->type = 2; // STT_FUNC
        sym->size = 0;
    }
    
    // 文字列テーブル構築
    char strtab[1024] = {0};
    int strtab_size = 1;
    
    char shstrtab[256] = {0};
    int shstrtab_size = 1;
    
    int shstr_null = shstrtab_size;
    shstrtab_size += sprintf(shstrtab + shstrtab_size, ".shstrtab") + 1;
    int shstr_text = shstrtab_size;
    shstrtab_size += sprintf(shstrtab + shstrtab_size, ".text") + 1;
    int shstr_rela = shstrtab_size;
    shstrtab_size += sprintf(shstrtab + shstrtab_size, ".rela.text") + 1;
    int shstr_symtab = shstrtab_size;
    shstrtab_size += sprintf(shstrtab + shstrtab_size, ".symtab") + 1;
    int shstr_strtab = shstrtab_size;
    shstrtab_size += sprintf(shstrtab + shstrtab_size, ".strtab") + 1;
    
    // シンボル名のオフセット計算
    for (int i = 0; i < as->symbol_count; i++) {
        as->symbols[i].value = (i == 0) ? 0 : strtab_size;
        if (strlen(as->symbols[i].name) > 0) {
            strcpy(strtab + strtab_size, as->symbols[i].name);
            strtab_size += strlen(as->symbols[i].name) + 1;
        }
    }
    
    // セクション計算
    uint64_t offset = 64; // ELFヘッダ後
    
    uint64_t text_offset = offset;
    offset += as->size;
    
    uint64_t rela_offset = offset;
    uint64_t rela_size = as->reloc_count * 24;
    offset += rela_size;
    
    uint64_t symtab_offset = offset;
    uint64_t symtab_size = as->symbol_count * 24;
    offset += symtab_size;
    
    uint64_t strtab_offset = offset;
    offset += strtab_size;
    
    uint64_t shstrtab_offset = offset;
    offset += shstrtab_size;
    
    uint64_t shoff = offset;
    
    // ELFヘッダ更新
    *(uint64_t*)(elf_header + 40) = shoff;
    *(uint16_t*)(elf_header + 60) = 6; // セクション数
    *(uint16_t*)(elf_header + 62) = 5; // shstrtab index
    
    fwrite(elf_header, 1, 64, out);
    
    // .textセクション
    fwrite(as->code, 1, as->size, out);
    
    // .rela.textセクション
    for (int i = 0; i < as->reloc_count; i++) {
        uint64_t r_offset = as->relocs[i].offset;
        uint64_t r_info = ((uint64_t)2 << 32) | as->relocs[i].type; // シンボル2 (外部)
        int64_t r_addend = as->relocs[i].addend;
        
        fwrite(&r_offset, 8, 1, out);
        fwrite(&r_info, 8, 1, out);
        fwrite(&r_addend, 8, 1, out);
    }
    
    // .symtabセクション
    for (int i = 0; i < as->symbol_count; i++) {
        uint32_t st_name = as->symbols[i].value;
        uint8_t st_info = (as->symbols[i].binding << 4) | as->symbols[i].type;
        uint8_t st_other = 0;
        uint16_t st_shndx = as->symbols[i].section;
        uint64_t st_value = (i == 0 || i == 1) ? 0 : as->symbols[i].value;
        uint64_t st_size = as->symbols[i].size;
        
        fwrite(&st_name, 4, 1, out);
        fwrite(&st_info, 1, 1, out);
        fwrite(&st_other, 1, 1, out);
        fwrite(&st_shndx, 2, 1, out);
        fwrite(&st_value, 8, 1, out);
        fwrite(&st_size, 8, 1, out);
    }
    
    // .strtab
    fwrite(strtab, 1, strtab_size, out);
    
    // .shstrtab
    fwrite(shstrtab, 1, shstrtab_size, out);
    
    // セクションヘッダテーブル
    uint8_t shdr[64] = {0};
    
    // NULL
    fwrite(shdr, 64, 1, out);
    
    // .text
    memset(shdr, 0, 64);
    *(uint32_t*)(shdr + 0) = shstr_text;
    *(uint32_t*)(shdr + 4) = 1; // SHT_PROGBITS
    *(uint64_t*)(shdr + 8) = 6; // SHF_ALLOC | SHF_EXECINSTR
    *(uint64_t*)(shdr + 24) = text_offset;
    *(uint64_t*)(shdr + 32) = as->size;
    *(uint64_t*)(shdr + 48) = 16; // align
    fwrite(shdr, 64, 1, out);
    
    // .rela.text
    memset(shdr, 0, 64);
    *(uint32_t*)(shdr + 0) = shstr_rela;
    *(uint32_t*)(shdr + 4) = 4; // SHT_RELA
    *(uint64_t*)(shdr + 24) = rela_offset;
    *(uint64_t*)(shdr + 32) = rela_size;
    *(uint32_t*)(shdr + 40) = 3; // link to symtab
    *(uint32_t*)(shdr + 44) = 1; // info: .text
    *(uint64_t*)(shdr + 48) = 8;
    *(uint64_t*)(shdr + 56) = 24; // entsize
    fwrite(shdr, 64, 1, out);
    
    // .symtab
    memset(shdr, 0, 64);
    *(uint32_t*)(shdr + 0) = shstr_symtab;
    *(uint32_t*)(shdr + 4) = 2; // SHT_SYMTAB
    *(uint64_t*)(shdr + 24) = symtab_offset;
    *(uint64_t*)(shdr + 32) = symtab_size;
    *(uint32_t*)(shdr + 40) = 4; // link to strtab
    *(uint32_t*)(shdr + 44) = 2; // info
    *(uint64_t*)(shdr + 48) = 8;
    *(uint64_t*)(shdr + 56) = 24;
    fwrite(shdr, 64, 1, out);
    
    // .strtab
    memset(shdr, 0, 64);
    *(uint32_t*)(shdr + 0) = shstr_strtab;
    *(uint32_t*)(shdr + 4) = 3; // SHT_STRTAB
    *(uint64_t*)(shdr + 24) = strtab_offset;
    *(uint64_t*)(shdr + 32) = strtab_size;
    *(uint64_t*)(shdr + 48) = 1;
    fwrite(shdr, 64, 1, out);
    
    // .shstrtab
    memset(shdr, 0, 64);
    *(uint32_t*)(shdr + 0) = shstr_null;
    *(uint32_t*)(shdr + 4) = 3; // SHT_STRTAB
    *(uint64_t*)(shdr + 24) = shstrtab_offset;
    *(uint64_t*)(shdr + 32) = shstrtab_size;
    *(uint64_t*)(shdr + 48) = 1;
    fwrite(shdr, 64, 1, out);
}

int main(int argc, char **argv) {
    if (argc < 3) {
        fprintf(stderr, "使用法: %s <入力.asm> <出力.o>\n", argv[0]);
        return 1;
    }
    
    FILE *in = fopen(argv[1], "r");
    if (!in) {
        perror("入力ファイルを開けません");
        return 1;
    }
    
    Assembler as = {0};
    char line[MAX_LINE];
    
    // パス1: ラベル収集
    while (fgets(line, sizeof(line), in)) {
        char tmp[MAX_LINE];
        strcpy(tmp, line);
        parse_line(&as, tmp, 1);
    }
    
    // パス2: コード生成
    rewind(in);
    as.size = 0;
    while (fgets(line, sizeof(line), in)) {
        parse_line(&as, line, 2);
    }
    fclose(in);
    
    // オブジェクトファイル出力
    FILE *out = fopen(argv[2], "wb");
    if (!out) {
        perror("出力ファイルを開けません");
        return 1;
    }
    
    write_elf_object(out, &as);
    fclose(out);
    
    printf("アセンブル完了: %zu バイト生成\n", as.size);
    printf("シンボル: %d, 再配置: %d\n", as.symbol_count, as.reloc_count);
    return 0;
}