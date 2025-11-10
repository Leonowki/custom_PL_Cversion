#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>

/* ----------------- Typedefinitions for Enums ----------------- */

typedef enum
{
    TOK_INT,
    TOK_CHAR,
    TOK_IDENT,
    TOK_NUMBER,
    TOK_CHAR_LITERAL,
    TOK_UNARY_OP,
    TOK_ASSIGN,
    TOK_PLUS_ASSIGN,
    TOK_MINUS_ASSIGN,
    TOK_MULT_ASSIGN,
    TOK_DIV_ASSIGN,
    TOK_PLUS,
    TOK_MINUS,
    TOK_MULT,
    TOK_DIV,
    TOK_SEMICOLON,
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_COMMA,
    TOK_INCREMENT,
    TOK_DECREMENT,
    TOK_EOF,
    TOK_ERROR
} TokenType;

typedef enum
{
    TYPE_INT,
    TYPE_CHAR,
    TYPE_ERROR,
    TYPE_UNKNOWN
} DataType;

typedef enum
{
    NODE_VAR_DECL,
    NODE_ASSIGN,
    NODE_BIN_OP,
    NODE_NUMBER,
    NODE_IDENT,
    NODE_PROGRAM,
    NODE_STATEMENT_LIST,
    NODE_DECL_LIST,
    NODE_CHAR_LITERAL,
    NODE_UNARY_OP,
    NODE_COMMA_EXPR
} NodeType;

typedef enum
{
    TAC_ASSIGN,
    TAC_ADD,
    TAC_SUB,
    TAC_MUL,
    TAC_DIV,
    TAC_COPY,
    TAC_INC,
    TAC_DEC,
} TACType;

typedef enum
{
    MIPS_DADDIU,
    MIPS_DADDU,
    MIPS_DSUBU,
    MIPS_DMULT,
    MIPS_DDIV,
    MIPS_MFLO,
    MIPS_SD,
    MIPS_SB,
    MIPS_LD,
    MIPS_LB,
} MIPSInstructionType;

/* ----------------- Struct Definitions ----------------- */

typedef struct
{
    TokenType type;
    char value[32];
    int line;
} Token;

typedef struct Symbol
{
    char *name;
    DataType type;
    int declared;
    int line;
    int memory_offset;
    struct Symbol *next;
} Symbol;

typedef struct ASTNode
{
    NodeType type;
    Token token;
    DataType data_type;
    struct ASTNode *left;
    struct ASTNode *right;
} ASTNode;

typedef struct ErrorInfo
{
    int line;
    char message[256];
    struct ErrorInfo *next;
} ErrorInfo;

typedef struct TACNode
{
    TACType type;
    char *result;
    char *arg1;
    char *arg2;
    struct TACNode *prev;
    struct TACNode *next;
} TACNode;

typedef struct MIPSInstruction
{
    MIPSInstructionType type;
    char *rt;
    char *rs;
    char *rd;
    char *base;
    char *offset;
    char *immediate;
    struct MIPSInstruction *prev;
    struct MIPSInstruction *next;
} MIPSInstruction;

/* ----------------- Program Configuration and Global Vars ----------------- */

bool DEBUG_MODE = true;
char *input;
int position = 0;
int current_line = 1;
Symbol *symbol_table = NULL;
int semantic_errors = 0;
Token current_token;
int current_memory_offset = 0;
int temp_registers_used[10] = {0};
char *temp_reg_names[10] = {"t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9"};
int temp_counter = 0;
ErrorInfo *error_list = NULL;
int error_count = 0;
MIPSInstruction *assembly_code = NULL;

/* ----------------- Function Prototypes ----------------- */

char *read_file(char *filename);

void process_file(char *filename);

void init_lexer(const char *input_str);
Token get_next_token();
void advance_token();
Token create_token(TokenType type, const char *value);
int is_identifier_char(char c);

int expect_token(TokenType expected);
void parser_error(const char *message);
ASTNode *create_ast_node(NodeType type, Token token, ASTNode *left, ASTNode *right);
void print_ast(ASTNode *node, int depth);
void free_ast(ASTNode *node);
void print_indent(int depth);

/**
 * Parser Grammar (CFG) — As Implemented
 *
 * program → stmt_list TOK_EOF
 *
 * stmt_list → stmt stmt_list | ε
 *
 * stmt → type init_list TOK_SEMICOLON | assignment TOK_SEMICOLON | unary_stmt TOK_SEMICOLON | TOK_SEMICOLON
 *
 * type → TOK_INT | TOK_CHAR
 *
 * init_list → init | init TOK_COMMA init_list
 *
 * init → TOK_IDENT | TOK_IDENT TOK_ASSIGN assignment_expression
 *
 * assignment → assignment_expression
 *
 * assignment_expression → additive_expression | TOK_IDENT assign_op comma_expression
 *
 * assign_op → TOK_ASSIGN | TOK_PLUS_ASSIGN | TOK_MINUS_ASSIGN | TOK_MULT_ASSIGN | TOK_DIV_ASSIGN
 *
 * comma_expression → assignment_expression | assignment_expression TOK_COMMA comma_expression
 *
 * additive_expression → term | additive_expression TOK_PLUS term | additive_expression TOK_MINUS term
 *
 * term → factor | term TOK_MULT factor | term TOK_DIV factor
 *
 * factor → TOK_NUMBER | TOK_CHAR_LITERAL | postfix_expression | TOK_LPAREN comma_expression TOK_RPAREN
 *          | TOK_PLUS factor | TOK_MINUS factor | TOK_INCREMENT TOK_IDENT | TOK_DECREMENT TOK_IDENT
 *
 * postfix_expression → TOK_IDENT | TOK_IDENT TOK_INCREMENT | TOK_IDENT TOK_DECREMENT
 *
 * unary_stmt → TOK_INCREMENT TOK_IDENT | TOK_DECREMENT TOK_IDENT
 */

ASTNode *parse_program();
ASTNode *parse_stmt_list();
ASTNode *parse_stmt();
ASTNode *parse_declaration();
ASTNode *parse_init_list(Token type_token);
ASTNode *parse_init(Token type_token);
ASTNode *parse_assignment();
ASTNode *parse_expression();
ASTNode *parse_assignment_expression();
ASTNode *parse_comma_expression();
ASTNode *parse_additive_expression();
ASTNode *parse_additive_tail(ASTNode *left);
ASTNode *parse_term();
ASTNode *parse_term_tail(ASTNode *left);
ASTNode *parse_factor();
ASTNode *parse_postfix_expression();

void init_symbol_table();
Symbol *lookup_symbol(const char *name);
Symbol *insert_symbol(const char *name, DataType type, int line);
void print_symbol_table();
void free_symbol_table();
int get_type_size(DataType type);
DataType get_type_from_token(Token token);
void semantic_analysis(ASTNode *node);
void check_declaration(ASTNode *node);
void check_assignment(ASTNode *node);
void check_expression(ASTNode *node);
DataType get_expression_type(ASTNode *node);
void check_unary_operation(ASTNode *node);

void print_errors();
void clear_errors();

TACNode *generate_tac(ASTNode *ast);
TACNode *create_tac_node(TACType type, const char *result, const char *arg1, const char *arg2);
void print_tac(TACNode *tac);
void free_tac(TACNode *tac);
TACNode *generate_tac_expr(ASTNode *node, TACNode **tac_tail);
TACNode *generate_tac_decl(ASTNode *node, TACNode **tac_tail);
TACNode *generate_tac_assign(ASTNode *node, TACNode **tac_tail);

char *get_temp_register();
void free_temp_register(char *reg_name);

void record_error(int line, const char *message);
void reset_temp_registers();

void process_stmt_list(ASTNode *stmt_list, TACNode **tac_tail);

MIPSInstruction *create_mips_instruction(MIPSInstructionType type, const char *rt, const char *rs, const char *rd, const char *base, const char *offset, const char *immediate);
int get_var_memory_address(char *var_name);
void append_mips_instruction(MIPSInstruction **head, MIPSInstruction **tail, MIPSInstruction *new_instr);
void free_mips_instructions(MIPSInstruction *head);
bool is_register(char *name);
MIPSInstruction *generate_assembly_code(TACNode *tac);
const char *get_mips_mnemonic(MIPSInstructionType type);
void write_assembly_to_file(MIPSInstruction *head, const char *filename);
char *get_register_binary(char *reg_name);
void decimal_to_binary(int n, char *binary_str, int bits);
void write_machine_code_to_file(MIPSInstruction *head, const char *filename_binary, const char *filename_hex);

/* --------------------- MAIN FUNCTION --------------------- */
int main(int argc, char *argv[])
{
    const char *fname = "test.txt";
    if (argc > 1)
        fname = argv[1];

    process_file((char *)fname);
    return 0;
}

void record_error(int line, const char *message)
{
    ErrorInfo *new_error = malloc(sizeof(ErrorInfo));
    if (!new_error)
    {
        fprintf(stderr, "Error: Failed to allocate memory for error info\n");
        return;
    }

    new_error->line = line;
    strncpy(new_error->message, message, sizeof(new_error->message) - 1);
    new_error->message[sizeof(new_error->message) - 1] = '\0';
    new_error->next = NULL;

    if (error_list == NULL)
    {
        error_list = new_error;
    }
    else
    {
        ErrorInfo *current = error_list;
        while (current->next != NULL)
        {
            current = current->next;
        }
        current->next = new_error;
    }

    error_count++;
}

void print_errors()
{
    if (error_count == 0)
    {
        printf("No errors found.\n");
        return;
    }

    printf("\n------------------ ERRORS (%d found) ------------------\n", error_count);
    ErrorInfo *current = error_list;
    while (current != NULL)
    {
        printf("Line %d: %s\n", current->line, current->message);
        current = current->next;
    }
}

void clear_errors()
{
    ErrorInfo *current = error_list;
    while (current != NULL)
    {
        ErrorInfo *next = current->next;
        free(current);
        current = next;
    }
    error_list = NULL;
    error_count = 0;
}

char *get_temp_register()
{
    for (int i = 0; i < 10; i++)
    {
        if (!temp_registers_used[i])
        {
            temp_registers_used[i] = 1;
            temp_counter++;
            return temp_reg_names[i];
        }
    }
    temp_registers_used[0] = 1;
    temp_counter++;
    return temp_reg_names[0];
}

void free_temp_register(char *reg_name)
{
    for (int i = 0; i < 10; i++)
    {
        if (strcmp(temp_reg_names[i], reg_name) == 0)
        {
            temp_registers_used[i] = 0;
            break;
        }
    }
}

void reset_temp_registers()
{
    for (int i = 0; i < 10; i++)
    {
        temp_registers_used[i] = 0;
    }
    temp_counter = 0;
}

void init_symbol_table()
{
    free_symbol_table();
    symbol_table = NULL;
    current_memory_offset = 0;
    semantic_errors = 0;
}

int get_type_size(DataType type)
{
    switch (type)
    {
    case TYPE_INT:
        return 4;
    case TYPE_CHAR:
        return 1;
    default:
        return 8;
    }
}

DataType get_type_from_token(Token token)
{
    if (token.type == TOK_INT)
        return TYPE_INT;
    if (token.type == TOK_CHAR)
        return TYPE_CHAR;
    return TYPE_UNKNOWN;
}

Symbol *lookup_symbol(const char *name)
{
    Symbol *current = symbol_table;
    while (current != NULL)
    {
        if (strcmp(current->name, name) == 0)
        {
            return current;
        }
        current = current->next;
    }
    return NULL;
}

Symbol *insert_symbol(const char *name, DataType type, int line)
{
    Symbol *existing = lookup_symbol(name);
    if (existing != NULL)
    {
        char error_msg[256];
        snprintf(error_msg, sizeof(error_msg),
                 "Redeclaration of variable '%s' (previously declared at line %d)",
                 name, existing->line);
        record_error(line, error_msg);
        semantic_errors++;
        return existing;
    }

    Symbol *new_symbol = malloc(sizeof(Symbol));
    if (!new_symbol)
    {
        fprintf(stderr, "Error: Failed to allocate memory for symbol\n");
        return NULL;
    }

    new_symbol->name = strdup(name);
    if (!new_symbol->name)
    {
        free(new_symbol);
        fprintf(stderr, "Error: Failed to allocate memory for symbol name\n");
        return NULL;
    }

    new_symbol->type = type;
    new_symbol->declared = 1;
    new_symbol->line = line;

    int type_size = get_type_size(type);

    if (current_memory_offset % 8 != 0)
    {
        current_memory_offset += 8 - (current_memory_offset % 8);
    }
    new_symbol->memory_offset = current_memory_offset;
    current_memory_offset += type_size;

    new_symbol->next = symbol_table;
    symbol_table = new_symbol;

    return new_symbol;
}

void print_symbol_table()
{
    printf("\n===== SYMBOL TABLE =====\n");
    printf("%-20s %-10s %-8s %-10s %s\n",
           "Name", "Type", "Line", "Offset", "Size");
    printf("------------------------------------------------------------\n");

    Symbol *current = symbol_table;
    int total_memory = 0;

    while (current != NULL)
    {
        const char *type_str = (current->type == TYPE_INT) ? "int" : (current->type == TYPE_CHAR) ? "char"
                                                                                                  : "unknown";
        int size = get_type_size(current->type);
        printf("%-20s %-10s %-8d %-10d %d bytes\n",
               current->name, type_str, current->line,
               current->memory_offset, size);
        total_memory += size;
        current = current->next;
    }

    printf("------------------------------------------------------------\n");
    printf("Total memory allocated: %d bytes\n", current_memory_offset);
    printf("Variables count: ");

    int count = 0;
    current = symbol_table;
    while (current != NULL)
    {
        count++;
        current = current->next;
    }
    printf("%d\n", count);
}

void free_symbol_table()
{
    Symbol *current = symbol_table;
    while (current != NULL)
    {
        Symbol *next = current->next;
        free(current->name);
        free(current);
        current = next;
    }
    symbol_table = NULL;
}

void semantic_analysis(ASTNode *node)
{
    if (node == NULL)
        return;

    switch (node->type)
    {
    case NODE_VAR_DECL:
        check_declaration(node);
        break;
    case NODE_ASSIGN:
        check_assignment(node);
        break;
    case NODE_IDENT:
        if (lookup_symbol(node->token.value) == NULL)
        {
            char error_msg[256];
            snprintf(error_msg, sizeof(error_msg),
                     "Undeclared variable '%s'", node->token.value);
            record_error(node->token.line, error_msg);
            semantic_errors++;
        }
        break;
    case NODE_BIN_OP:
        check_expression(node);
        break;
    case NODE_UNARY_OP:
        check_unary_operation(node);
        break;
    case NODE_COMMA_EXPR:
        semantic_analysis(node->left);
        semantic_analysis(node->right);
        return;
    }

    semantic_analysis(node->left);
    semantic_analysis(node->right);
}

void check_unary_operation(ASTNode *node)
{
    if (node->type != NODE_UNARY_OP || node->left == NULL)
        return;

    if (node->left->type != NODE_IDENT)
    {
        record_error(node->token.line, "Increment/decrement requires a variable");
        semantic_errors++;
        return;
    }

    Symbol *symbol = lookup_symbol(node->left->token.value);
    if (symbol == NULL)
    {
        char error_msg[256];
        snprintf(error_msg, sizeof(error_msg),
                 "Undeclared variable '%s' in increment/decrement",
                 node->left->token.value);
        record_error(node->left->token.line, error_msg);
        semantic_errors++;
        return;
    }

    if (symbol->type != TYPE_INT && symbol->type != TYPE_CHAR)
    {
        record_error(node->token.line, "Increment/decrement requires numeric type");
        semantic_errors++;
    }
}

void check_declaration(ASTNode *node)
{
    if (node->type != NODE_VAR_DECL || node->left == NULL)
        return;

    DataType var_type = get_type_from_token(node->token);

    if (var_type == TYPE_UNKNOWN)
    {
        record_error(node->token.line, "Unknown type in declaration");
        semantic_errors++;
        return;
    }

    Symbol *symbol = insert_symbol(node->left->token.value, var_type, node->left->token.line);

    if (node->right != NULL)
    {
        DataType expr_type = get_expression_type(node->right);
        if (expr_type != TYPE_ERROR && expr_type != var_type)
        {
            if (!(var_type == TYPE_CHAR && expr_type == TYPE_INT || var_type == TYPE_INT && expr_type == TYPE_CHAR))
            {
                char error_msg[256];
                const char *expected_type = (var_type == TYPE_INT) ? "int" : "char";
                const char *actual_type = (expr_type == TYPE_INT) ? "int" : "char";
                snprintf(error_msg, sizeof(error_msg),
                         "Type mismatch in initialization: expected %s, got %s",
                         expected_type, actual_type);
                record_error(node->right->token.line, error_msg);
                semantic_errors++;
            }
        }
    }
}

void check_assignment(ASTNode *node)
{
    if (node->type != NODE_ASSIGN || node->left == NULL || node->right == NULL)
        return;

    if (node->left->type != NODE_IDENT)
    {
        record_error(node->token.line, "Assignment to non-variable");
        semantic_errors++;
        return;
    }

    Symbol *symbol = lookup_symbol(node->left->token.value);
    if (symbol == NULL)
    {
        char error_msg[256];
        snprintf(error_msg, sizeof(error_msg),
                 "Assignment to undeclared variable '%s'",
                 node->left->token.value);
        record_error(node->left->token.line, error_msg);
        semantic_errors++;
        return;
    }

    DataType expr_type = get_expression_type(node->right);
    if (expr_type != TYPE_ERROR && expr_type != symbol->type)
    {
        if (!((symbol->type == TYPE_INT && expr_type == TYPE_CHAR) ||
              (symbol->type == TYPE_CHAR && expr_type == TYPE_INT)))
        {
            char error_msg[256];
            const char *expected_type = (symbol->type == TYPE_INT) ? "int" : "char";
            const char *actual_type = (expr_type == TYPE_INT) ? "int" : "char";
            snprintf(error_msg, sizeof(error_msg),
                     "Type mismatch in assignment to '%s': expected %s, got %s",
                     symbol->name, expected_type, actual_type);
            record_error(node->right->token.line, error_msg);
            semantic_errors++;
        }
    }
}

void check_expression(ASTNode *node)
{
    if (node->type != NODE_BIN_OP)
        return;

    DataType left_type = get_expression_type(node->left);
    DataType right_type = get_expression_type(node->right);

    if (node->token.type == TOK_DIV)
    {
        if (node->right->type == NODE_NUMBER && strcmp(node->right->token.value, "0") == 0)
        {
            record_error(node->token.line, "Division by zero in constant expression");
            semantic_errors++;
            return;
        }
    }

    if (left_type != TYPE_ERROR && right_type != TYPE_ERROR && left_type != right_type)
    {
        if (!((left_type == TYPE_INT && right_type == TYPE_CHAR) ||
              (left_type == TYPE_CHAR && right_type == TYPE_INT)))
        {
            char error_msg[256];
            const char *left_str = (left_type == TYPE_INT) ? "int" : "char";
            const char *right_str = (right_type == TYPE_INT) ? "int" : "char";
            snprintf(error_msg, sizeof(error_msg),
                     "Type mismatch in expression: %s and %s",
                     left_str, right_str);
            record_error(node->token.line, error_msg);
            semantic_errors++;
        }
    }
}

DataType get_expression_type(ASTNode *node)
{
    if (node == NULL)
        return TYPE_ERROR;

    switch (node->type)
    {
    case NODE_IDENT:
    {
        Symbol *symbol = lookup_symbol(node->token.value);
        if (symbol == NULL)
        {
            return TYPE_ERROR;
        }
        return symbol->type;
    }
    case NODE_NUMBER:
        return TYPE_INT;
    case NODE_CHAR_LITERAL:
        return TYPE_CHAR;
    case NODE_BIN_OP:
    {
        DataType left_type = get_expression_type(node->left);
        DataType right_type = get_expression_type(node->right);

        if (left_type == TYPE_INT || right_type == TYPE_INT)
        {
            return TYPE_INT;
        }
        if (left_type == TYPE_CHAR && right_type == TYPE_CHAR)
        {
            return TYPE_CHAR;
        }
        return TYPE_ERROR;
    }
    case NODE_UNARY_OP:
        return get_expression_type(node->left);
    case NODE_ASSIGN:
        return get_expression_type(node->left);
    case NODE_COMMA_EXPR:
        return get_expression_type(node->right);
    default:
        return TYPE_ERROR;
    }
}

TACNode *create_tac_node(TACType type, const char *result, const char *arg1, const char *arg2)
{
    TACNode *node = malloc(sizeof(TACNode));
    if (!node)
        return NULL;

    node->type = type;
    node->result = result ? strdup(result) : NULL;
    node->arg1 = arg1 ? strdup(arg1) : NULL;
    node->arg2 = arg2 ? strdup(arg2) : NULL;
    node->prev = NULL;
    node->next = NULL;

    return node;
}

void print_tac(TACNode *tac)
{
    printf("\n===== THREE ADDRESS CODE =====\n");

    if (tac == NULL)
    {
        printf("No TAC generated\n");
        return;
    }

    TACNode *current = tac;
    while (current != NULL)
    {
        switch (current->type)
        {
        case TAC_ASSIGN:
            printf("%s = %s\n", current->result, current->arg1);
            break;
        case TAC_ADD:
            printf("%s = %s + %s\n", current->result, current->arg1, current->arg2);
            break;
        case TAC_SUB:
            printf("%s = %s - %s\n", current->result, current->arg1, current->arg2);
            break;
        case TAC_MUL:
            printf("%s = %s * %s\n", current->result, current->arg1, current->arg2);
            break;
        case TAC_DIV:
            printf("%s = %s / %s\n", current->result, current->arg1, current->arg2);
            break;
        case TAC_COPY:
            printf("%s = %s\n", current->result, current->arg1);
            break;
        case TAC_INC:
            printf("%s = %s + %s\n", current->result, current->arg1, current->arg2);
            break;
        case TAC_DEC:
            printf("%s = %s - %s\n", current->result, current->arg1, current->arg2);
            break;
        default:
            printf("Unknown TAC instruction\n");
        }
        current = current->next;
    }
}

void free_tac(TACNode *tac)
{
    TACNode *current = tac;
    while (current != NULL)
    {
        TACNode *next = current->next;
        if (current->result)
            free(current->result);
        if (current->arg1)
            free(current->arg1);
        if (current->arg2)
            free(current->arg2);
        free(current);
        current = next;
    }
}

TACNode *generate_tac_expr(ASTNode *node, TACNode **tac_tail)
{
    if (node == NULL)
        return NULL;

    switch (node->type)
    {
    case NODE_IDENT:
    {
        char *temp = get_temp_register();
        TACNode *tac = create_tac_node(TAC_COPY, temp, node->token.value, NULL);

        if (*tac_tail)
        {
            (*tac_tail)->next = tac;
            tac->prev = *tac_tail;
        }
        *tac_tail = tac;
        return tac;
    }

    case NODE_NUMBER:
    case NODE_CHAR_LITERAL:
    {
        char *temp = get_temp_register();
        TACNode *tac = create_tac_node(TAC_ASSIGN, temp, node->token.value, NULL);

        if (*tac_tail)
        {
            (*tac_tail)->next = tac;
            tac->prev = *tac_tail;
        }
        *tac_tail = tac;
        return tac;
    }

    case NODE_BIN_OP:
    {
        TACNode *left_tac = generate_tac_expr(node->left, tac_tail);
        char *left_temp = left_tac ? left_tac->result : NULL;

        TACNode *right_tac = generate_tac_expr(node->right, tac_tail);
        char *right_temp = right_tac ? right_tac->result : NULL;

        if (!left_temp || !right_temp)
            return NULL;

        char *result_temp = get_temp_register();
        TACType op_type;

        switch (node->token.type)
        {
        case TOK_PLUS:
            op_type = TAC_ADD;
            break;
        case TOK_MINUS:
            op_type = TAC_SUB;
            break;
        case TOK_MULT:
            op_type = TAC_MUL;
            break;
        case TOK_DIV:
            op_type = TAC_DIV;
            break;
        default:
            return NULL;
        }

        TACNode *tac = create_tac_node(op_type, result_temp, left_temp, right_temp);

        free_temp_register(left_temp);
        free_temp_register(right_temp);

        if (*tac_tail)
        {
            (*tac_tail)->next = tac;
            tac->prev = *tac_tail;
        }
        *tac_tail = tac;
        return tac;
    }

    case NODE_ASSIGN:
    {
        TACNode *rhs_tac = generate_tac_expr(node->right, tac_tail);
        if (!rhs_tac)
            return NULL;

        char *lhs_name = node->left->token.value;
        TACNode *assign_tac = create_tac_node(TAC_COPY, lhs_name, rhs_tac->result, NULL);

        if (*tac_tail)
        {
            (*tac_tail)->next = assign_tac;
            assign_tac->prev = *tac_tail;
        }
        *tac_tail = assign_tac;

        return rhs_tac;
    }

    case NODE_COMMA_EXPR:
    {

        TACNode *left_tac = generate_tac_expr(node->left, tac_tail);

        if (left_tac && left_tac->result && is_register(left_tac->result))
        {
            free_temp_register(left_tac->result);
        }

        TACNode *right_tac = generate_tac_expr(node->right, tac_tail);

        return right_tac;
    }

    case NODE_UNARY_OP:
    {
        if (node->token.type == TOK_INCREMENT || node->token.type == TOK_DECREMENT)
        {
            if (node->left == NULL || node->left->type != NODE_IDENT)
                return NULL;

            char *var_name = node->left->token.value;
            char *temp = get_temp_register();

            TACNode *load_tac = create_tac_node(TAC_COPY, temp, var_name, NULL);
            if (*tac_tail)
            {
                (*tac_tail)->next = load_tac;
                load_tac->prev = *tac_tail;
            }
            *tac_tail = load_tac;

            TACType op_type = (node->token.type == TOK_INCREMENT) ? TAC_INC : TAC_DEC;
            TACNode *op_tac = create_tac_node(op_type, var_name, var_name, "1");

            if (*tac_tail)
            {
                (*tac_tail)->next = op_tac;
                op_tac->prev = *tac_tail;
            }
            *tac_tail = op_tac;

            return load_tac;
        }

        return NULL;
    }

    default:
        return NULL;
    }
}

TACNode *generate_tac_decl(ASTNode *node, TACNode **tac_tail)
{
    if (node->type != NODE_VAR_DECL || node->left == NULL)
        return NULL;

    char *var_name = node->left->token.value;

    if (node->right != NULL)
    {

        TACNode *expr_tac = generate_tac_expr(node->right, tac_tail);
        if (expr_tac)
        {
            TACNode *assign_tac = create_tac_node(TAC_COPY, var_name, expr_tac->result, NULL);

            free_temp_register(expr_tac->result);

            if (*tac_tail)
            {
                (*tac_tail)->next = assign_tac;
                assign_tac->prev = *tac_tail;
            }
            *tac_tail = assign_tac;
            return assign_tac;
        }
    }

    return NULL;
}

TACNode *generate_tac_assign(ASTNode *node, TACNode **tac_tail)
{
    if (node->type != NODE_ASSIGN || node->left == NULL || node->right == NULL)
        return NULL;

    TACNode *expr_tac = generate_tac_expr(node, tac_tail);

    if (expr_tac && expr_tac->result)
    {
        free_temp_register(expr_tac->result);
    }

    return expr_tac;
}

void process_stmt_list(ASTNode *stmt_list, TACNode **tac_tail)
{
    if (stmt_list == NULL)
        return;

    if (stmt_list->left != NULL)
    {
        ASTNode *stmt = stmt_list->left;

        switch (stmt->type)
        {
        case NODE_VAR_DECL:
            generate_tac_decl(stmt, tac_tail);
            break;

        case NODE_ASSIGN:
            generate_tac_assign(stmt, tac_tail);
            break;

        case NODE_UNARY_OP:
            generate_tac_expr(stmt, tac_tail);
            break;

        case NODE_DECL_LIST:
        {
            ASTNode *decl = stmt;
            while (decl != NULL)
            {
                if (decl->type == NODE_VAR_DECL)
                {
                    generate_tac_decl(decl, tac_tail);
                }
                else if (decl->type == NODE_DECL_LIST && decl->left != NULL)
                {
                    generate_tac_decl(decl->left, tac_tail);
                }
                decl = decl->right;
            }
            break;
        }
        }
    }

    if (stmt_list->right != NULL)
    {
        process_stmt_list(stmt_list->right, tac_tail);
    }
}

TACNode *generate_tac(ASTNode *ast)
{
    if (ast == NULL)
        return NULL;

    TACNode *tac_head = NULL;
    TACNode *tac_tail = NULL;

    reset_temp_registers();

    if (ast->type == NODE_PROGRAM && ast->left != NULL)
    {
        process_stmt_list(ast->left, &tac_tail);
    }
    else
    {

        process_stmt_list(ast, &tac_tail);
    }

    if (tac_tail != NULL)
    {
        tac_head = tac_tail;
        while (tac_head->prev != NULL)
        {
            tac_head = tac_head->prev;
        }
    }

    return tac_head;
}

MIPSInstruction *create_mips_instruction(MIPSInstructionType type, const char *rt, const char *rs, const char *rd, const char *base, const char *offset, const char *immediate)
{
    MIPSInstruction *instr = malloc(sizeof(MIPSInstruction));
    if (!instr)
        return NULL;

    instr->type = type;
    instr->rt = rt ? strdup(rt) : NULL;
    instr->rs = rs ? strdup(rs) : NULL;
    instr->rd = rd ? strdup(rd) : NULL;
    instr->base = base ? strdup(base) : NULL;
    instr->offset = offset ? strdup(offset) : NULL;
    instr->immediate = immediate ? strdup(immediate) : NULL;
    instr->prev = NULL;
    instr->next = NULL;

    return instr;
}

int get_var_memory_address(char *var_name)
{
    Symbol *symbol = lookup_symbol(var_name);
    if (symbol != NULL)
    {
        return symbol->memory_offset;
    }
    return 0;
}

void append_mips_instruction(MIPSInstruction **head, MIPSInstruction **tail, MIPSInstruction *new_instr)
{
    if (*head == NULL)
    {
        *head = *tail = new_instr;
    }
    else
    {
        (*tail)->next = new_instr;
        new_instr->prev = *tail;
        *tail = new_instr;
    }
}

void free_mips_instructions(MIPSInstruction *head)
{
    MIPSInstruction *temp;
    while (head)
    {
        temp = head;
        head = head->next;

        free(temp->rt);
        free(temp->rs);
        free(temp->rd);
        free(temp->base);
        free(temp->offset);
        free(temp->immediate);
        free(temp);
    }
}

bool is_register(char *name)
{
    if (strcmp(name, "t0") == 0 || strcmp(name, "t1") == 0 || strcmp(name, "t2") == 0 ||
        strcmp(name, "t3") == 0 || strcmp(name, "t4") == 0 || strcmp(name, "t5") == 0 ||
        strcmp(name, "t6") == 0 || strcmp(name, "t7") == 0 || strcmp(name, "t8") == 0 ||
        strcmp(name, "t9") == 0)
    {
        return true;
    }
    else
        return false;
}

MIPSInstruction *generate_assembly_code(TACNode *tac)
{
    if (tac == NULL)
    {
        printf("No TAC generated\n");
        return NULL;
    }

    TACNode *current = tac;
    MIPSInstruction *head = NULL;
    MIPSInstruction *tail = NULL;

    while (current != NULL)
    {
        MIPSInstruction *instr = NULL;

        switch (current->type)
        {
        case TAC_ASSIGN:
            if (isalpha(current->arg1[0]) || (current->arg1[0] == '\\' && current->arg1[1] != '\0'))
            {
                char imm_str[12];
                int char_value;

                if (current->arg1[0] == '\\' && current->arg1[1] != '\0')
                {
                    switch (current->arg1[1])
                    {
                    case 'n':
                        char_value = '\n';
                        break;
                    case 't':
                        char_value = '\t';
                        break;
                    case 'r':
                        char_value = '\r';
                        break;
                    case '0':
                        char_value = '\0';
                        break;
                    case '\\':
                        char_value = '\\';
                        break;
                    case '\'':
                        char_value = '\'';
                        break;
                    case '\"':
                        char_value = '\"';
                        break;
                    case 'a':
                        char_value = '\a';
                        break;
                    case 'b':
                        char_value = '\b';
                        break;
                    case 'f':
                        char_value = '\f';
                        break;
                    case 'v':
                        char_value = '\v';
                        break;
                    default:
                        char_value = current->arg1[1];
                        break;
                    }
                    sprintf(imm_str, "%d", char_value);
                }
                else
                {

                    sprintf(imm_str, "%d", (int)current->arg1[0]);
                }
                instr = create_mips_instruction(MIPS_DADDIU, current->result, "zero", NULL, NULL, NULL, imm_str);
            }
            else
            {

                instr = create_mips_instruction(MIPS_DADDIU, current->result, "zero", NULL, NULL, NULL, current->arg1);
            }
            break;

        case TAC_INC:
        {
            char offset_str[12];
            sprintf(offset_str, "%d", get_var_memory_address(current->result));

            char *temp_reg = get_temp_register();
            MIPSInstruction *load = create_mips_instruction(
                MIPS_LD, temp_reg, NULL, NULL, "zero", offset_str, NULL);
            append_mips_instruction(&head, &tail, load);

            MIPSInstruction *add = create_mips_instruction(
                MIPS_DADDIU, temp_reg, temp_reg, NULL, NULL, NULL, "1");
            append_mips_instruction(&head, &tail, add);

            instr = create_mips_instruction(
                MIPS_SD, temp_reg, NULL, NULL, "zero", offset_str, NULL);

            free_temp_register(temp_reg);
        }
        break;

        case TAC_DEC:
        {
            char offset_str[12];
            sprintf(offset_str, "%d", get_var_memory_address(current->result));

            char *temp_reg = get_temp_register();
            MIPSInstruction *load = create_mips_instruction(
                MIPS_LD, temp_reg, NULL, NULL, "zero", offset_str, NULL);
            append_mips_instruction(&head, &tail, load);

            MIPSInstruction *sub = create_mips_instruction(
                MIPS_DADDIU, temp_reg, temp_reg, NULL, NULL, NULL, "-1");
            append_mips_instruction(&head, &tail, sub);

            instr = create_mips_instruction(
                MIPS_SD, temp_reg, NULL, NULL, "zero", offset_str, NULL);

            free_temp_register(temp_reg);
        }
        break;

        case TAC_COPY:
        {
            char offset_str[12];

            if (is_register(current->arg1))
            {
                sprintf(offset_str, "%d", get_var_memory_address(current->result));
                instr = create_mips_instruction(
                    MIPS_SD,
                    current->arg1,
                    NULL, NULL,
                    "zero",
                    offset_str,
                    NULL);
            }
            else
            {
                sprintf(offset_str, "%d", get_var_memory_address(current->arg1));
                instr = create_mips_instruction(
                    MIPS_LD,
                    current->result,
                    NULL, NULL,
                    "zero",
                    offset_str,
                    NULL);
            }
        }
        break;

        case TAC_ADD:
        {
            char *reg1 = current->arg1;
            char *reg2 = current->arg2;
            bool free_reg1 = false;
            bool free_reg2 = false;

            if (!is_register(current->arg1))
            {
                reg1 = get_temp_register();
                free_reg1 = true;
                char offset_str[12];
                sprintf(offset_str, "%d", get_var_memory_address(current->arg1));
                MIPSInstruction *load1 = create_mips_instruction(
                    MIPS_LD, reg1, NULL, NULL, "zero", offset_str, NULL);
                append_mips_instruction(&head, &tail, load1);
            }

            if (!is_register(current->arg2))
            {
                reg2 = get_temp_register();
                free_reg2 = true;
                char offset_str[12];
                sprintf(offset_str, "%d", get_var_memory_address(current->arg2));
                MIPSInstruction *load2 = create_mips_instruction(
                    MIPS_LD, reg2, NULL, NULL, "zero", offset_str, NULL);
                append_mips_instruction(&head, &tail, load2);
            }

            instr = create_mips_instruction(MIPS_DADDU, reg2, reg1, current->result, NULL, NULL, NULL);

            if (free_reg1)
                free_temp_register(reg1);
            if (free_reg2)
                free_temp_register(reg2);
        }
        break;

        case TAC_SUB:
        {
            char *reg1 = current->arg1;
            char *reg2 = current->arg2;
            bool free_reg1 = false;
            bool free_reg2 = false;

            if (!is_register(current->arg1))
            {
                reg1 = get_temp_register();
                free_reg1 = true;
                char offset_str[12];
                sprintf(offset_str, "%d", get_var_memory_address(current->arg1));
                MIPSInstruction *load1 = create_mips_instruction(
                    MIPS_LD, reg1, NULL, NULL, "zero", offset_str, NULL);
                append_mips_instruction(&head, &tail, load1);
            }

            if (!is_register(current->arg2))
            {
                reg2 = get_temp_register();
                free_reg2 = true;
                char offset_str[12];
                sprintf(offset_str, "%d", get_var_memory_address(current->arg2));
                MIPSInstruction *load2 = create_mips_instruction(
                    MIPS_LD, reg2, NULL, NULL, "zero", offset_str, NULL);
                append_mips_instruction(&head, &tail, load2);
            }

            instr = create_mips_instruction(MIPS_DSUBU, reg2, reg1, current->result, NULL, NULL, NULL);

            if (free_reg1)
                free_temp_register(reg1);
            if (free_reg2)
                free_temp_register(reg2);
        }
        break;

        case TAC_MUL:
        {
            char *reg1 = current->arg1;
            char *reg2 = current->arg2;
            bool free_reg1 = false;
            bool free_reg2 = false;

            if (!is_register(current->arg1))
            {
                reg1 = get_temp_register();
                free_reg1 = true;
                char offset_str[12];
                sprintf(offset_str, "%d", get_var_memory_address(current->arg1));
                MIPSInstruction *load1 = create_mips_instruction(
                    MIPS_LD, reg1, NULL, NULL, "zero", offset_str, NULL);
                append_mips_instruction(&head, &tail, load1);
            }

            if (!is_register(current->arg2))
            {
                reg2 = get_temp_register();
                free_reg2 = true;
                char offset_str[12];
                sprintf(offset_str, "%d", get_var_memory_address(current->arg2));
                MIPSInstruction *load2 = create_mips_instruction(
                    MIPS_LD, reg2, NULL, NULL, "zero", offset_str, NULL);
                append_mips_instruction(&head, &tail, load2);
            }

            MIPSInstruction *mult = create_mips_instruction(MIPS_DMULT, reg2, reg1, NULL, NULL, NULL, NULL);
            append_mips_instruction(&head, &tail, mult);
            instr = create_mips_instruction(MIPS_MFLO, NULL, NULL, current->result, NULL, NULL, NULL);

            if (free_reg1)
                free_temp_register(reg1);
            if (free_reg2)
                free_temp_register(reg2);
        }
        break;

        case TAC_DIV:
        {
            char *reg1 = current->arg1;
            char *reg2 = current->arg2;
            bool free_reg1 = false;
            bool free_reg2 = false;

            if (!is_register(current->arg1))
            {
                reg1 = get_temp_register();
                free_reg1 = true;
                char offset_str[12];
                sprintf(offset_str, "%d", get_var_memory_address(current->arg1));
                MIPSInstruction *load1 = create_mips_instruction(
                    MIPS_LD, reg1, NULL, NULL, "zero", offset_str, NULL);
                append_mips_instruction(&head, &tail, load1);
            }

            if (!is_register(current->arg2))
            {
                reg2 = get_temp_register();
                free_reg2 = true;
                char offset_str[12];
                sprintf(offset_str, "%d", get_var_memory_address(current->arg2));
                MIPSInstruction *load2 = create_mips_instruction(
                    MIPS_LD, reg2, NULL, NULL, "zero", offset_str, NULL);
                append_mips_instruction(&head, &tail, load2);
            }

            MIPSInstruction *div = create_mips_instruction(MIPS_DDIV, reg2, reg1, NULL, NULL, NULL, NULL);
            append_mips_instruction(&head, &tail, div);
            instr = create_mips_instruction(MIPS_MFLO, NULL, NULL, current->result, NULL, NULL, NULL);

            if (free_reg1)
                free_temp_register(reg1);
            if (free_reg2)
                free_temp_register(reg2);
        }
        break;

        default:
            break;
        }

        if (instr != NULL)
        {
            append_mips_instruction(&head, &tail, instr);
        }

        current = current->next;
    }

    return head;
}

const char *get_mips_mnemonic(MIPSInstructionType type)
{
    switch (type)
    {
    case MIPS_DADDIU:
        return "daddiu";
    case MIPS_DADDU:
        return "daddu";
    case MIPS_DSUBU:
        return "dsubu";
    case MIPS_DMULT:
        return "dmult";
    case MIPS_DDIV:
        return "ddiv";
    case MIPS_MFLO:
        return "mflo";
    case MIPS_SD:
        return "sd";
    case MIPS_SB:
        return "sb";
    case MIPS_LD:
        return "ld";
    case MIPS_LB:
        return "lb";
    default:
        return "unknown";
    }
}

void write_assembly_to_file(MIPSInstruction *head, const char *filename)
{
    FILE *fp = fopen(filename, "w");
    if (!fp)
    {
        perror("Error opening file for writing assembly code");
        return;
    }

    fprintf(fp, ".data\n\n.code\n\n");

    MIPSInstruction *curr = head;
    while (curr)
    {
        const char *mnemonic = get_mips_mnemonic(curr->type);

        switch (curr->type)
        {
        case MIPS_DADDU:
            fprintf(fp, "%s $%s, $%s, $%s\n", mnemonic, curr->rd, curr->rs, curr->rt);
            break;
        case MIPS_DSUBU:
            fprintf(fp, "%s $%s, $%s, $%s\n", mnemonic, curr->rd, curr->rs, curr->rt);
            break;

        case MIPS_DADDIU:
            fprintf(fp, "%s $%s, $%s, %s\n", mnemonic, curr->rt, curr->rs, curr->immediate);
            break;

        case MIPS_SD:
            fprintf(fp, "%s $%s, %s($%s)\n", mnemonic, curr->rt, curr->offset, curr->base);
            break;
        case MIPS_SB:
            fprintf(fp, "%s $%s, %s($%s)\n", mnemonic, curr->rt, curr->offset, curr->base);
            break;
        case MIPS_LD:
            fprintf(fp, "%s $%s, %s($%s)\n", mnemonic, curr->rt, curr->offset, curr->base);
            break;
        case MIPS_LB:
            fprintf(fp, "%s $%s, %s($%s)\n", mnemonic, curr->rt, curr->offset, curr->base);
            break;
        case MIPS_DMULT:
            fprintf(fp, "%s $%s, $%s\n", mnemonic, curr->rs, curr->rt);
            break;
        case MIPS_DDIV:
            fprintf(fp, "%s $%s, $%s\n", mnemonic, curr->rs, curr->rt);
            break;
        case MIPS_MFLO:
            fprintf(fp, "%s $%s\n", mnemonic, curr->rd);
            break;

        default:
            fprintf(fp, "%s\n", mnemonic);
            break;
        }

        curr = curr->next;
    }

    fclose(fp);
    printf("MIPS assembly written to %s\n", filename);
}

char *get_register_binary(char *reg_name)
{
    if (strcmp(reg_name, "zero") == 0)
        return "00000";
    else if (strcmp(reg_name, "t0") == 0)
        return "01000";
    else if (strcmp(reg_name, "t1") == 0)
        return "01001";
    else if (strcmp(reg_name, "t2") == 0)
        return "01010";
    else if (strcmp(reg_name, "t3") == 0)
        return "01011";
    else if (strcmp(reg_name, "t4") == 0)
        return "01100";
    else if (strcmp(reg_name, "t5") == 0)
        return "01101";
    else if (strcmp(reg_name, "t6") == 0)
        return "01110";
    else if (strcmp(reg_name, "t7") == 0)
        return "01111";
    else if (strcmp(reg_name, "t8") == 0)
        return "11000";
    else if (strcmp(reg_name, "t9") == 0)
        return "11001";
    else
        return "00000";
}

void decimal_to_binary(int n, char *binary_str, int bits)
{
    binary_str[bits] = '\0';
    for (int i = bits - 1; i >= 0; i--)
    {
        binary_str[i] = (n & 1) ? '1' : '0';
        n >>= 1;
    }
}

void write_machine_code_to_file(MIPSInstruction *head, const char *filename_binary, const char *filename_hex)
{
    FILE *fp = fopen(filename_binary, "w");
    FILE *fph = NULL;
    if (strlen(filename_hex) > 0)
    {
        printf("Hex Output Set True\n");
        fph = fopen(filename_hex, "w");
        if (!fph)
        {
            perror("Error opening file for writing hex code");
            fclose(fp);
            return;
        }
    }
    if (!fp)
    {
        perror("Error opening file for writing assembly code");
        return;
    }

    MIPSInstruction *curr = head;
    while (curr)
    {
        char *binary_rep = malloc(33);

        switch (curr->type)
        {
        case MIPS_DADDU:
        {
            char *op_code = "000000";
            char *rs = get_register_binary(curr->rs);
            char *rt = get_register_binary(curr->rt);
            char *rd = get_register_binary(curr->rd);
            char *sa = "00000";
            char *funct = "101101";
            sprintf(binary_rep, "%s%s%s%s%s%s", op_code, rd, rs, rt, sa, funct);
            break;
        }
        case MIPS_DSUBU:
        {
            char *op_code = "000000";
            char *rs = get_register_binary(curr->rs);
            char *rt = get_register_binary(curr->rt);
            char *rd = get_register_binary(curr->rd);
            char *sa = "00000";
            char *funct = "101111";
            sprintf(binary_rep, "%s%s%s%s%s%s", op_code, rd, rs, rt, sa, funct);
            break;
        }
        case MIPS_DADDIU:
        {
            char *op_code = "011001";
            char *rs = get_register_binary(curr->rs);
            char *rt = get_register_binary(curr->rt);
            char immediate[33];
            decimal_to_binary(atoi(curr->immediate), immediate, 16);
            sprintf(binary_rep, "%s%s%s%s", op_code, rs, rt, immediate);
            break;
        }
        case MIPS_SD:
        {
            char *op_code = "111111";
            char *base = get_register_binary(curr->base);
            char *rt = get_register_binary(curr->rt);
            char offset[33];
            decimal_to_binary(atoi(curr->offset), offset, 16);
            sprintf(binary_rep, "%s%s%s%s", op_code, base, rt, offset);
            break;
        }
        case MIPS_SB:
        {
            char *op_code = "101000";
            char *base = get_register_binary(curr->base);
            char *rt = get_register_binary(curr->rt);
            char offset[33];
            decimal_to_binary(atoi(curr->offset), offset, 16);
            sprintf(binary_rep, "%s%s%s%s", op_code, base, rt, offset);
            break;
        }
        case MIPS_LD:
        {
            char *op_code = "110111";
            char *base = get_register_binary(curr->base);
            char *rt = get_register_binary(curr->rt);
            char offset[33];
            decimal_to_binary(atoi(curr->offset), offset, 16);
            sprintf(binary_rep, "%s%s%s%s", op_code, base, rt, offset);
            break;
        }
        case MIPS_LB:
        {
            char *op_code = "100000";
            char *base = get_register_binary(curr->base);
            char *rt = get_register_binary(curr->rt);
            char offset[33];
            decimal_to_binary(atoi(curr->offset), offset, 16);
            sprintf(binary_rep, "%s%s%s%s", op_code, base, rt, offset);
            break;
        }
        case MIPS_DMULT:
        {
            char *op_code = "000000";
            char *rs = get_register_binary(curr->rs);
            char *rt = get_register_binary(curr->rt);
            char *sa = "0000000000";
            char *funct = "011100";
            sprintf(binary_rep, "%s%s%s%s%s", op_code, rs, rt, sa, funct);
            break;
        }
        case MIPS_DDIV:
        {
            char *op_code = "000000";
            char *rs = get_register_binary(curr->rs);
            char *rt = get_register_binary(curr->rt);
            char *sa = "0000000000";
            char *funct = "011111";
            sprintf(binary_rep, "%s%s%s%s%s", op_code, rs, rt, sa, funct);
            break;
        }
        case MIPS_MFLO:
        {
            char *op_code = "000000";
            char *sa = "0000000000";
            char *rd = get_register_binary(curr->rd);
            char *something = "00000";
            char *funct = "011111";
            sprintf(binary_rep, "%s%s%s%s%s", op_code, sa, rd, something, funct);
            break;
        }
        default:
            sprintf(binary_rep, "00000000000000000000000000000000\n");
            break;
        }
        fprintf(fp, "%s\n", binary_rep);

        if (fph)
        {
            unsigned long hex_rep = strtoul(binary_rep, NULL, 2);
            fprintf(fph, "0x%08lX\n", hex_rep);
        }

        curr = curr->next;
    }

    fclose(fp);

    printf("Machine code written to %s\n", filename_binary);

    if (fph)
    {
        fclose(fph);
        printf("Machine code in hex format written to %s\n", filename_hex);
    }
}

void parser_error(const char *message)
{
    record_error(current_token.line, message);
}

int expect_token(TokenType expected)
{
    if (current_token.type == expected)
    {
        advance_token();
        return 1;
    }

    char error_msg[256];
    const char *expected_str =
        (expected == TOK_INT)            ? "TOK_INT"
        : (expected == TOK_CHAR)         ? "TOK_CHAR"
        : (expected == TOK_IDENT)        ? "TOK_IDENT"
        : (expected == TOK_NUMBER)       ? "TOK_NUMBER"
        : (expected == TOK_CHAR_LITERAL) ? "TOK_CHAR_LITERAL"
        : (expected == TOK_ASSIGN)       ? "TOK_ASSIGN"
        : (expected == TOK_PLUS_ASSIGN)  ? "TOK_PLUS_ASSIGN"
        : (expected == TOK_MINUS_ASSIGN) ? "TOK_MINUS_ASSIGN"
        : (expected == TOK_MULT_ASSIGN)  ? "TOK_MULT_ASSIGN"
        : (expected == TOK_DIV_ASSIGN)   ? "TOK_DIV_ASSIGN"
        : (expected == TOK_PLUS)         ? "TOK_PLUS"
        : (expected == TOK_MINUS)        ? "TOK_MINUS"
        : (expected == TOK_MULT)         ? "TOK_MULT"
        : (expected == TOK_DIV)          ? "TOK_DIV"
        : (expected == TOK_SEMICOLON)    ? "TOK_SEMICOLON"
        : (expected == TOK_LPAREN)       ? "TOK_LPAREN"
        : (expected == TOK_RPAREN)       ? "TOK_RPAREN"
        : (expected == TOK_COMMA)        ? "TOK_COMMA"
        : (expected == TOK_INCREMENT)    ? "TOK_INCREMENT"
        : (expected == TOK_DECREMENT)    ? "TOK_DECREMENT"
        : (expected == TOK_EOF)          ? "TOK_EOF"
                                         : "TOK_ERROR";

    snprintf(error_msg, sizeof(error_msg), "Expected %s but found %s",
             expected_str,
             (current_token.type == TOK_INT)            ? "TOK_INT"
             : (current_token.type == TOK_CHAR)         ? "TOK_CHAR"
             : (current_token.type == TOK_IDENT)        ? "TOK_IDENT"
             : (current_token.type == TOK_NUMBER)       ? "TOK_NUMBER"
             : (current_token.type == TOK_CHAR_LITERAL) ? "TOK_CHAR_LITERAL"
             : (current_token.type == TOK_ASSIGN)       ? "TOK_ASSIGN"
             : (current_token.type == TOK_PLUS)         ? "TOK_PLUS"
             : (current_token.type == TOK_MINUS)        ? "TOK_MINUS"
             : (current_token.type == TOK_MULT)         ? "TOK_MULT"
             : (current_token.type == TOK_DIV)          ? "TOK_DIV"
             : (current_token.type == TOK_SEMICOLON)    ? "TOK_SEMICOLON"
             : (current_token.type == TOK_LPAREN)       ? "TOK_LPAREN"
             : (current_token.type == TOK_RPAREN)       ? "TOK_RPAREN"
             : (current_token.type == TOK_COMMA)        ? "TOK_COMMA"
             : (current_token.type == TOK_INCREMENT)    ? "TOK_INCREMENT"
             : (current_token.type == TOK_DECREMENT)    ? "TOK_DECREMENT"
             : (current_token.type == TOK_EOF)          ? "TOK_EOF"
                                                        : "TOK_ERROR");

    parser_error(error_msg);
    return 0;
}

ASTNode *create_ast_node(NodeType type, Token token, ASTNode *left, ASTNode *right)
{
    ASTNode *node = malloc(sizeof(ASTNode));
    if (!node)
    {
        fprintf(stderr, "Error: Failed to Allocate Memory for AST Node\n");
        return NULL;
    }
    node->type = type;
    node->token = token;
    node->data_type = TYPE_UNKNOWN;
    node->left = left;
    node->right = right;
    return node;
}

void print_indent(int depth)
{
    for (int i = 0; i < depth; ++i)
        printf("  ");
}

void print_ast(ASTNode *node, int depth)
{
    if (node == NULL)
        return;

    print_indent(depth);
    const char *names[] = {
        "VAR_DECL", "ASSIGN", "BIN_OP", "NUMBER", "IDENT",
        "PROGRAM", "STMT_LIST", "DECL_LIST", "CHAR_LITERAL", "UNARY_OP", "COMMA_EXPR"};
    if (node->type >= 0 && node->type <= NODE_COMMA_EXPR)
        printf("%s", names[node->type]);
    else
        printf("NODE?");

    if (node->type == NODE_IDENT || node->type == NODE_NUMBER ||
        node->type == NODE_BIN_OP || node->type == NODE_VAR_DECL ||
        node->type == NODE_CHAR_LITERAL || node->type == NODE_UNARY_OP ||
        node->type == NODE_COMMA_EXPR)
    {
        printf(" [%s]", node->token.value);
    }
    printf("\n");

    print_ast(node->left, depth + 1);
    print_ast(node->right, depth + 1);
}

void free_ast(ASTNode *node)
{
    if (!node)
        return;
    free_ast(node->left);
    free_ast(node->right);
    free(node);
}

ASTNode *parse_program()
{
    ASTNode *stmts = parse_stmt_list();
    if (current_token.type != TOK_EOF)
    {
        parser_error("Expected EOF after program");
    }
    ASTNode *program = create_ast_node(NODE_PROGRAM, create_token(TOK_EOF, NULL), stmts, NULL);
    return program;
}

ASTNode *parse_stmt_list()
{
    if (current_token.type == TOK_EOF)
    {
        return NULL;
    }

    ASTNode *first = parse_stmt();
    if (first == NULL)
    {

        while (current_token.type != TOK_EOF &&
               current_token.type != TOK_INT &&
               current_token.type != TOK_CHAR &&
               current_token.type != TOK_IDENT)
        {
            advance_token();
        }
        if (current_token.type != TOK_EOF)
        {
            return parse_stmt_list();
        }
        return NULL;
    }

    ASTNode *head = create_ast_node(NODE_STATEMENT_LIST, create_token(TOK_EOF, NULL), first, NULL);
    ASTNode *cur = head;

    while (current_token.type != TOK_EOF)
    {
        ASTNode *next = parse_stmt();
        if (next == NULL)
        {

            while (current_token.type != TOK_EOF &&
                   current_token.type != TOK_INT &&
                   current_token.type != TOK_CHAR &&
                   current_token.type != TOK_IDENT)
            {
                advance_token();
            }
            if (current_token.type == TOK_EOF)
                break;
            continue;
        }

        ASTNode *node = create_ast_node(NODE_STATEMENT_LIST, create_token(TOK_EOF, NULL), next, NULL);
        cur->right = node;
        cur = node;
    }

    return head;
}

ASTNode *parse_stmt()
{
    if (current_token.type == TOK_INT || current_token.type == TOK_CHAR)
    {
        Token type_token = current_token;
        advance_token();
        ASTNode *decls = parse_init_list(type_token);

        if (current_token.type == TOK_SEMICOLON)
        {
            while (current_token.type == TOK_SEMICOLON)
                advance_token();
        }
        else
        {
            parser_error("Expected ';' after declaration");
        }

        return decls;
    }
    else if (current_token.type == TOK_INCREMENT || current_token.type == TOK_DECREMENT)
    {
        ASTNode *unary_stmt = parse_factor();
        if (current_token.type == TOK_SEMICOLON)
            advance_token();
        else
            parser_error("Expected ';' after unary statement");
        return unary_stmt;
    }
    else if (current_token.type == TOK_IDENT)
    {
        ASTNode *assign_stmt = parse_assignment();

        if (current_token.type == TOK_SEMICOLON)
        {
            while (current_token.type == TOK_SEMICOLON)
                advance_token();
        }
        else
        {
            parser_error("Expected ';' after assignment");
        }

        return assign_stmt;
    }
    else if (current_token.type == TOK_SEMICOLON)
    {
        while (current_token.type == TOK_SEMICOLON)
            advance_token();
        return NULL;
    }
    else
    {
        parser_error("Unexpected token in statement");
        advance_token();
        return NULL;
    }
}

ASTNode *parse_init_list(Token type_token)
{

    ASTNode *first_decl = parse_init(type_token);
    if (first_decl == NULL)
    {
        return NULL;
    }

    if (current_token.type == TOK_COMMA)
    {
        ASTNode *head = create_ast_node(NODE_DECL_LIST, create_token(TOK_EOF, NULL), first_decl, NULL);
        ASTNode *cur = head;
        while (current_token.type == TOK_COMMA)
        {
            advance_token();
            ASTNode *next_decl = parse_init(type_token);
            if (next_decl == NULL)
            {

                while (current_token.type != TOK_EOF &&
                       current_token.type != TOK_SEMICOLON &&
                       current_token.type != TOK_IDENT)
                {
                    advance_token();
                }
                if (current_token.type == TOK_IDENT)
                {
                    next_decl = parse_init(type_token);
                }
                else
                {
                    break;
                }
            }

            if (next_decl != NULL)
            {
                ASTNode *node = create_ast_node(NODE_DECL_LIST, create_token(TOK_EOF, NULL), next_decl, NULL);
                cur->right = node;
                cur = node;
            }
        }
        return head;
    }
    else
    {
        return first_decl;
    }
}

ASTNode *parse_init(Token type_token)
{
    if (current_token.type != TOK_IDENT)
    {
        parser_error("Expected identifier in declaration");
        return NULL;
    }

    Token ident = current_token;
    advance_token();

    ASTNode *ident_node = create_ast_node(NODE_IDENT, ident, NULL, NULL);
    if (!ident_node)
        return NULL;

    ASTNode *decl_node = create_ast_node(NODE_VAR_DECL, type_token, ident_node, NULL);
    if (!decl_node)
    {
        free_ast(ident_node);
        return NULL;
    }

    if (current_token.type == TOK_ASSIGN)
    {
        advance_token();
        ASTNode *expr = parse_assignment_expression();
        if (expr != NULL)
        {
            decl_node->right = expr;
        }
    }

    return decl_node;
}

ASTNode *parse_assignment()
{
    ASTNode *node = parse_assignment_expression();
    if (!node)
        parser_error("Invalid assignment statement");
    return node;
}

ASTNode *parse_assignment_expression()
{
    ASTNode *left = parse_additive_expression();

    if (current_token.type == TOK_ASSIGN ||
        current_token.type == TOK_PLUS_ASSIGN ||
        current_token.type == TOK_MINUS_ASSIGN ||
        current_token.type == TOK_MULT_ASSIGN ||
        current_token.type == TOK_DIV_ASSIGN)
    {
        if (left->type != NODE_IDENT)
        {
            parser_error("Left-hand side of assignment must be a variable");
            free_ast(left);
            return NULL;
        }

        Token assign_tok = current_token;
        TokenType op_type = current_token.type;
        advance_token();

        ASTNode *right = parse_comma_expression();
        if (!right)
        {
            free_ast(left);
            return NULL;
        }

        if (op_type != TOK_ASSIGN)
        {
            Token bin_op_token;
            bin_op_token.line = assign_tok.line;

            switch (op_type)
            {
            case TOK_PLUS_ASSIGN:
                bin_op_token.type = TOK_PLUS;
                strcpy(bin_op_token.value, "+");
                break;
            case TOK_MINUS_ASSIGN:
                bin_op_token.type = TOK_MINUS;
                strcpy(bin_op_token.value, "-");
                break;
            case TOK_MULT_ASSIGN:
                bin_op_token.type = TOK_MULT;
                strcpy(bin_op_token.value, "*");
                break;
            case TOK_DIV_ASSIGN:
                bin_op_token.type = TOK_DIV;
                strcpy(bin_op_token.value, "/");
                break;
            default:
                break;
            }

            ASTNode *left_copy = create_ast_node(NODE_IDENT, left->token, NULL, NULL);
            ASTNode *bin_op = create_ast_node(NODE_BIN_OP, bin_op_token, left_copy, right);

            Token simple_assign = assign_tok;
            simple_assign.type = TOK_ASSIGN;
            strcpy(simple_assign.value, "=");

            return create_ast_node(NODE_ASSIGN, simple_assign, left, bin_op);
        }

        return create_ast_node(NODE_ASSIGN, assign_tok, left, right);
    }

    return left;
}

ASTNode *parse_comma_expression()
{
    ASTNode *left = parse_assignment_expression();
    if (!left)
        return NULL;

    if (current_token.type == TOK_COMMA)
    {

        Token comma_tok = current_token;
        advance_token();

        ASTNode *right = parse_comma_expression();
        if (!right)
        {

            return left;
        }

        return create_ast_node(NODE_COMMA_EXPR, comma_tok, left, right);
    }

    return left;
}

ASTNode *parse_additive_expression()
{
    ASTNode *left = parse_term();
    if (left == NULL)
    {
        return NULL;
    }
    return parse_additive_tail(left);
}

ASTNode *parse_additive_tail(ASTNode *left)
{
    while (current_token.type == TOK_PLUS || current_token.type == TOK_MINUS)
    {
        Token op = current_token;
        advance_token();
        ASTNode *right = parse_term();
        if (right == NULL)
        {
            return left;
        }
        ASTNode *bin = create_ast_node(NODE_BIN_OP, op, left, right);
        if (!bin)
        {
            free_ast(right);
            return left;
        }
        left = bin;
    }
    return left;
}

ASTNode *parse_term()
{
    ASTNode *left = parse_factor();
    if (left == NULL)
    {
        return NULL;
    }
    return parse_term_tail(left);
}

ASTNode *parse_term_tail(ASTNode *left)
{
    while (current_token.type == TOK_MULT || current_token.type == TOK_DIV)
    {
        Token op = current_token;
        advance_token();
        ASTNode *right = parse_factor();
        if (right == NULL)
        {
            return left;
        }
        ASTNode *bin = create_ast_node(NODE_BIN_OP, op, left, right);
        if (!bin)
        {
            free_ast(right);
            return left;
        }
        left = bin;
    }
    return left;
}

ASTNode *parse_factor()
{
    if (current_token.type == TOK_NUMBER)
    {
        Token t = current_token;
        advance_token();
        return create_ast_node(NODE_NUMBER, t, NULL, NULL);
    }
    else if (current_token.type == TOK_CHAR_LITERAL)
    {
        Token t = current_token;
        advance_token();
        return create_ast_node(NODE_CHAR_LITERAL, t, NULL, NULL);
    }
    else if (current_token.type == TOK_INCREMENT || current_token.type == TOK_DECREMENT)
    {
        Token op_token = current_token;
        advance_token();

        if (current_token.type != TOK_IDENT)
        {
            parser_error("Expected identifier after prefix ++ or --");
            return NULL;
        }

        ASTNode *id_node = create_ast_node(NODE_IDENT, current_token, NULL, NULL);
        ASTNode *unary_node = create_ast_node(NODE_UNARY_OP, op_token, id_node, NULL);

        advance_token();
        return unary_node;
    }
    else if (current_token.type == TOK_IDENT)
    {
        return parse_postfix_expression();
    }
    else if (current_token.type == TOK_LPAREN)
    {
        advance_token();

        ASTNode *expr = parse_comma_expression();
        if (expr == NULL)
        {
            while (current_token.type != TOK_EOF && current_token.type != TOK_RPAREN)
            {
                advance_token();
            }
            if (current_token.type == TOK_RPAREN)
            {
                advance_token();
            }
            return NULL;
        }
        return expr;
    }
    else if (current_token.type == TOK_PLUS)
    {
        advance_token();
        ASTNode *inner = parse_factor();
        return inner;
    }
    else if (current_token.type == TOK_MINUS)
    {
        Token op = current_token;
        advance_token();
        ASTNode *right = parse_factor();
        if (right == NULL)
        {
            return NULL;
        }
        Token zero_tok = create_token(TOK_NUMBER, "0");
        ASTNode *zero_node = create_ast_node(NODE_NUMBER, zero_tok, NULL, NULL);
        if (!zero_node)
        {
            free_ast(right);
            return NULL;
        }
        ASTNode *bin = create_ast_node(NODE_BIN_OP, op, zero_node, right);
        if (!bin)
        {
            free_ast(zero_node);
            free_ast(right);
            return NULL;
        }
        return bin;
    }
    else
    {
        parser_error("Unexpected token in factor");
        return NULL;
    }
}

ASTNode *parse_postfix_expression()
{
    if (current_token.type != TOK_IDENT)
    {
        parser_error("Expected identifier in postfix expression");
        return NULL;
    }

    Token ident = current_token;
    advance_token();

    ASTNode *ident_node = create_ast_node(NODE_IDENT, ident, NULL, NULL);

    if (current_token.type == TOK_INCREMENT || current_token.type == TOK_DECREMENT)
    {
        Token op = current_token;
        advance_token();

        ASTNode *unary_node = create_ast_node(NODE_UNARY_OP, op, ident_node, NULL);
        return unary_node;
    }

    return ident_node;
}

ASTNode *parse_expression()
{
    return parse_assignment_expression();
}

void init_lexer(const char *input_str)
{
    if (input)
        free(input);
    input = strdup(input_str);
    position = 0;
    current_line = 1;
}

Token create_token(TokenType type, const char *value)
{
    Token token;
    token.type = type;
    if (value)
    {
        strncpy(token.value, value, sizeof(token.value) - 1);
        token.value[sizeof(token.value) - 1] = '\0';
    }
    else
    {
        token.value[0] = '\0';
    }
    token.line = current_line;
    return token;
}

int is_identifier_char(char c)
{
    return isalnum((unsigned char)c) || c == '_';
}

Token get_next_token()
{

    while (isspace((unsigned char)input[position]))
    {
        if (input[position] == '\n')
            current_line++;
        position++;
    }

    if (input[position] == '/')
    {
        if (input[position + 1] == '/')
        {
            position += 2;
            while (input[position] != '\0' && input[position] != '\n')
                position++;
            return get_next_token();
        }

        if (input[position + 1] == '*')
        {
            position += 2;
            while (input[position] != '\0')
            {
                if (input[position] == '*' && input[position + 1] == '/')
                {
                    position += 2;
                    break;
                }
                if (input[position] == '\n')
                    current_line++;
                position++;
            }
            return get_next_token();
        }

        if (input[position + 1] == '=')
        {
            position += 2;
            return create_token(TOK_DIV_ASSIGN, "/=");
        }

        position++;
        return create_token(TOK_DIV, "/");
    }

    if (input[position] == '\0')
    {
        return create_token(TOK_EOF, NULL);
    }

    if (input[position] == '+')
    {
        if (input[position + 1] == '+')
        {
            position += 2;
            return create_token(TOK_INCREMENT, "++");
        }
        if (input[position + 1] == '=')
        {
            position += 2;
            return create_token(TOK_PLUS_ASSIGN, "+=");
        }
        position++;
        return create_token(TOK_PLUS, "+");
    }

    if (input[position] == '-')
    {
        if (input[position + 1] == '-')
        {
            position += 2;
            return create_token(TOK_DECREMENT, "--");
        }
        if (input[position + 1] == '=')
        {
            position += 2;
            return create_token(TOK_MINUS_ASSIGN, "-=");
        }
        position++;
        return create_token(TOK_MINUS, "-");
    }

    if (input[position] == '*')
    {
        if (input[position + 1] == '=')
        {
            position += 2;
            return create_token(TOK_MULT_ASSIGN, "*=");
        }
        position++;
        return create_token(TOK_MULT, "*");
    }

    if (input[position] == '\'')
    {
        position++;
        char char_val[4] = {0};
        int i = 0;

        if (input[position] == '\\')
        {
            char_val[i++] = input[position++];
            if (i < 3)
                char_val[i++] = input[position++];
        }
        else if (input[position] != '\'' && input[position] != '\0')
        {
            char_val[i++] = input[position++];
        }

        if (input[position] != '\'')
        {
            return create_token(TOK_ERROR, "Unterminated character literal");
        }
        position++;

        return create_token(TOK_CHAR_LITERAL, char_val);
    }

    if (isalpha((unsigned char)input[position]) || input[position] == '_')
    {
        char ident[32] = {0};
        int i = 0;

        while (is_identifier_char(input[position]) && i < 31)
        {
            ident[i++] = input[position++];
        }
        ident[i] = '\0';

        if (strcmp(ident, "int") == 0)
            return create_token(TOK_INT, ident);
        if (strcmp(ident, "char") == 0)
            return create_token(TOK_CHAR, ident);

        return create_token(TOK_IDENT, ident);
    }

    if (isdigit((unsigned char)input[position]))
    {
        char number[32] = {0};
        int i = 0;

        while (isdigit((unsigned char)input[position]) && i < 31)
        {
            number[i++] = input[position++];
        }
        number[i] = '\0';

        return create_token(TOK_NUMBER, number);
    }

    char current = input[position++];

    switch (current)
    {
    case '=':
        return create_token(TOK_ASSIGN, "=");
    case ';':
        return create_token(TOK_SEMICOLON, ";");
    case '(':
        return create_token(TOK_LPAREN, "(");
    case ')':
        return create_token(TOK_RPAREN, ")");
    case ',':
        return create_token(TOK_COMMA, ",");
    }

    char error[2] = {current, '\0'};
    return create_token(TOK_ERROR, error);
}

void advance_token()
{
    current_token = get_next_token();
}

char *read_file(char *filename)
{
    FILE *file = fopen(filename, "rb");
    if (file == NULL)
    {
        fprintf(stderr, "Error: Cannot open file '%s'\n", filename);
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    if (file_size <= 0)
    {
        fprintf(stderr, "Error: File '%s' is empty or cannot be read\n", filename);
        fclose(file);
        return NULL;
    }

    char *buffer = malloc(file_size + 1);
    if (buffer == NULL)
    {
        fprintf(stderr, "Error: Memory allocation failed\n");
        fclose(file);
        return NULL;
    }

    size_t bytes_read = fread(buffer, 1, file_size, file);
    if (bytes_read != (size_t)file_size)
    {
        fprintf(stderr, "Warning: only read %zu of %ld bytes\n", bytes_read, file_size);
    }

    buffer[bytes_read] = '\0';
    fclose(file);
    return buffer;
}

void process_file(char *filename)
{
    printf("Processing file: %s\n", filename);
    char *source_code = read_file(filename);
    if (source_code == NULL)
        return;

    init_lexer(source_code);
    init_symbol_table();
    advance_token();
    ASTNode *ast = parse_program();
    TACNode *tac = NULL;

    semantic_analysis(ast);

    if (error_count > 0)
    {
        print_errors();
    }
    else
    {
        tac = generate_tac(ast);
        if (tac)
        {
            assembly_code = generate_assembly_code(tac);
            if (assembly_code)
            {
                write_assembly_to_file(assembly_code, "output_mips64.txt");
                write_machine_code_to_file(assembly_code, "output_machine_code_binary.txt", "output_machine_code_hex.txt");
            }
        }
    }

    if (DEBUG_MODE)
    {
        printf("DEBUG MODE TRUE: Printing AST , SYMBOL TABLE AND TAC.\n");
        printf("\n===== AST =====\n");
        print_ast(ast, 0);
        print_symbol_table();
        print_errors();

        if (error_count == 0)
        {
            print_tac(tac);
        }
    }

    free_ast(ast);
    free_tac(tac);
    free_mips_instructions(assembly_code);
    free_symbol_table();
    free(source_code);
    clear_errors();
}