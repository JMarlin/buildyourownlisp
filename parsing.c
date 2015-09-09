#include <stdio.h>
#include <stdlib.h>
#include "mpc.h"

/* If we are compiling on Windows compile these functions */
#ifdef _WIN32
#include <string.h>

static char buffer[2048];

/* Fake readline function */
char* readline(char* prompt) {

    fputs(prompt, stdout);
    fgets(buffer, 2048, stdin);
    char* cpy = malloc(strlen(buffer) + 1);
    strcpy(cpy, buffer);
    cpy[strlen(cpy) - 1] = '\0';
    return cpy;
}

/* Fake add_history function */
void add_history(char* unused) {}

/* Otherwise include the editline headers */
#else
#include <editline/readline.h>
#endif

#define LASSERT(args, cond, err) \
    if(!(cond)) { lval_del(args); return lval_err(err); }

/* Declare lval struct */
typedef struct lval {
    int type;
    long inum;
    double fnum;
    char* err;
    char* sym;
    int count;
    struct lval** cell;
} lval;

/* Create Enumeration of Possible lval types */
enum { LVAL_INT, LVAL_FLOAT, LVAL_ERR, LVAL_SYM, LVAL_SEXPR, LVAL_QEXPR };

/* Create a integer type lval */
lval* lval_int(long x) {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_INT;
    v->inum = x;
    return v;
}

/* Create a float type lval */
lval* lval_float(float x) {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_FLOAT;
    v->fnum = x;
    return v;
}

/* Create a new error type lval */
lval* lval_err(char* msg) {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_ERR;
    v->err = malloc(strlen(msg) + 1);
    strcpy(v->err, msg);
    return v;
}

/* Create a new Symbol lval */
lval* lval_sym(char* s) {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_SYM;
    v->sym = malloc(strlen(s) + 1);
    strcpy(v->sym, s);
    return v;
}

/* A pointer to a new empty Sexpr lval */
lval* lval_sexpr(void) {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_SEXPR;
    v->count = 0;
    v->cell = NULL;
    return v;
}

/* A pointer to a new empty Qexpr lval */
lval* lval_qexpr(void) {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_QEXPR;
    v->count = 0;
    v->cell = NULL;
    return v;
}


void lval_del(lval* v) {

    switch(v->type) {
        /* Do nothing special for number types */
        case LVAL_FLOAT:
        case LVAL_INT:
            break;

        /* For Err or Sym free the string data */
        case LVAL_ERR: free(v->err); break;
        case LVAL_SYM: free(v->sym); break;

        /* If Qexpr or Sexpr then delete all elements inside */
        case LVAL_QEXPR:
        case LVAL_SEXPR:
            for(int i = 0; i < v->count; i++)
                lval_del(v->cell[i]);

            /*Also free the memory allocated to contain the pointers */
            free(v->cell);
        break;
    }

    /* Free the memory allocated for the "lval" struct itself */
    free(v);
}

lval* lval_read_int(mpc_ast_t* t) {
    errno = 0;
    long x = strtol(t->contents, NULL, 10);
    return errno != ERANGE ?
        lval_int(x) : lval_err("Invalid integer");
}

lval* lval_read_float(mpc_ast_t* t) {
    errno = 0;
    double x = strtod(t->contents, NULL);
    return errno != ERANGE ?
        lval_float(x) : lval_err("Invalid float");
}

lval* lval_add(lval* v, lval* x) {
    v->count++;
    v->cell = realloc(v->cell, sizeof(lval*) * v->count);
    v->cell[v->count - 1] = x;
    return v;
}

lval* lval_read(mpc_ast_t* t) {

    /* If Symbol or Number return conversion to that type */
    if (strstr(t->tag, "integer")) return lval_read_int(t);
    if (strstr(t->tag, "float")) return lval_read_float(t);
    if (strstr(t->tag, "symbol")) return lval_sym(t->contents);

    /* If root (>) or sexpr then create empty list */
    lval* x = NULL;
    if(strcmp(t->tag, ">") == 0 || strstr(t->tag, "sexpr")) x = lval_sexpr();
    if(strstr(t->tag, "qexpr")) x= lval_qexpr();

    /* Fill in this list with any valid expression contained within */
    for(int i = 0; i < t->children_num; i++) {
        if(strcmp(t->children[i]->contents, "(") == 0 ||
           strcmp(t->children[i]->contents, ")") == 0 ||
           strcmp(t->children[i]->contents, "{") == 0 ||
           strcmp(t->children[i]->contents, "}") == 0 ||
           strcmp(t->children[i]->tag, "regex") == 0)
            continue;
        lval_add(x, lval_read(t->children[i]));
    }

    return x;
}

void lval_expr_print(lval* v, char open, char close);

void lval_print(lval* v) {
    switch(v->type) {
        case LVAL_INT:   printf("%li", v->inum); break;
        case LVAL_FLOAT: printf("%f", v->fnum); break;
        case LVAL_ERR:   printf("Error: %s", v->err); break;
        case LVAL_SYM:   printf(v->sym); break;
        case LVAL_SEXPR: lval_expr_print(v, '(', ')'); break;
        case LVAL_QEXPR: lval_expr_print(v, '{', '}'); break;
    }
}

void lval_expr_print(lval* v, char open, char close) {
    putchar(open);
    for(int i = 0; i < v->count; i++) {

        /* Print value contained within */
        lval_print(v->cell[i]);

        /* Don't print trailing space if last element */
        if(i != (v->count - 1))
            putchar(' ');
    }
    putchar(close);
}

/* Cast an integer lval to a float */
lval* lval_to_float(lval* source) {

    switch(source->type) {
        case LVAL_INT:
            source->type = LVAL_FLOAT;
            source->fnum = (double)source->inum;
            break;

        case LVAL_FLOAT:
        case LVAL_ERR:
        default:
            break;
    }

    return source;
}

/* Print an "lval" followed by a newline */
void lval_println(lval* v) {
    lval_print(v);
    putchar('\n');
}

lval* eval_op_float(lval* x, char* op, lval* y, int arg_count) {

        lval* ret_val = 0;

        if(x->type != LVAL_FLOAT || (y && y->type != LVAL_FLOAT)) {

            ret_val = lval_err("Argument type mismatch!");
        } else {

            if(strcmp(op, "+") == 0) ret_val = arg_count < 2 ? lval_err("Too few arguments") : lval_float(x->fnum + y->fnum);
            if(strcmp(op, "-") == 0) ret_val = arg_count < 2 ? lval_float(-x->fnum) : lval_float(x->fnum - y->fnum);
            if(strcmp(op, "*") == 0) ret_val = arg_count < 2 ? lval_err("Too few arguments") : lval_float(x->fnum * y->fnum);
            if(strcmp(op, "/") == 0) ret_val = arg_count < 2 ? lval_err("Too few arguments") : (y->fnum == 0 ? lval_err("Division by zero") : lval_float(x->fnum / y->fnum));
            if(strcmp(op, "%") == 0) ret_val = arg_count < 2 ? lval_err("Too few arguments") : lval_err("Incorrect argument types!");
            if(strcmp(op, "^") == 0) ret_val = arg_count < 2 ? lval_err("Too few arguments") : lval_float((double)powf(x->fnum, y->fnum));
            if(strcmp(op, "min") == 0) ret_val = arg_count == 0 ? lval_err("Too few arguments") : (arg_count == 1 ? x : (x->fnum < y->fnum ? x : y));
            if(strcmp(op, "max") == 0) ret_val = arg_count == 0 ? lval_err("Too few arguments") : (arg_count == 1 ? x : (x->fnum > y->fnum ? x : y));
        }

        if(ret_val != x) lval_del(x);
        if(y && ret_val != y) lval_del(y);

        if(!ret_val)
            return lval_err("Invalid float operation");
        else
            return ret_val;
}

lval* eval_op_int(lval* x, char* op, lval* y, int arg_count) {

    lval* ret_val = 0;

    if(x->type != LVAL_INT || (y && y->type != LVAL_INT)) {

        ret_val = lval_err("Argument type mismatch!");
    } else {

        if(strcmp(op, "+") == 0) ret_val = arg_count < 2 ? lval_err("Too few arguments") : lval_int(x->inum + y->inum);
        if(strcmp(op, "-") == 0) ret_val = arg_count < 2 ? lval_int(-x->inum) : lval_int(x->inum - y->inum);
        if(strcmp(op, "*") == 0) ret_val = arg_count < 2 ? lval_err("Too few arguments") : lval_int(x->inum * y->inum);
        if(strcmp(op, "/") == 0) ret_val = arg_count < 2 ? lval_err("Too few arguments") : (y->inum == 0 ? lval_err("Division by zero") : lval_int(x->inum / y->inum));
        if(strcmp(op, "%") == 0) ret_val = arg_count < 2 ? lval_err("Too few arguments") : lval_int(x->inum % y->inum);
        if(strcmp(op, "^") == 0) ret_val = arg_count < 2 ? lval_err("Too few arguments") : lval_int(powl(x->inum, y->inum));
        if(strcmp(op, "min") == 0) ret_val = arg_count == 0 ? lval_err("Too few arguments") : (arg_count == 1 ? x : (x->inum < y->inum ? x : y));
        if(strcmp(op, "max") == 0) ret_val = arg_count == 0 ? lval_err("Too few arguments") : (arg_count == 1 ? x : (x->inum > y->inum ? x : y));
    }

    if(ret_val != x) lval_del(x);
    if(y && ret_val != y) lval_del(y);

    if(!ret_val)
        return lval_err("Invalid integer operation");
    else
        return ret_val;
}

lval* lval_eval(lval* v);

lval* lval_pop(lval* v, int i) {
    /* Find the item at "i" */
    lval* x = v->cell[i];

    /*Shift memory after the item at "i" over the top */
    memmove(&v->cell[i], &v->cell[i + 1], sizeof(lval*) * (v->count - i - 1));

    /* Decrease the count of items in the list */
    v->count--;

    /* Reallocate the memory used */
    v->cell = realloc(v->cell, sizeof(lval*) * v->count);
    return x;
}

void lval_push(lval* d, lval* s) {

    /* Increase the count of items in the list */
    d->count++;

    /* Reallocate the memory used */
    d->cell = realloc(d->cell, sizeof(lval*) * d->count);

    /*Shift memory after the item at "i" over the top */
    memmove(&d->cell[1], &d->cell[0], sizeof(lval*) * (d->count - 1));

    /*Insert the new item*/
    d->cell[0] = s;
}

lval* lval_take(lval* v, int i) {
    lval* x = lval_pop(v, i);
    lval_del(v);
    return x;
}

lval* builtin_len(lval* a) {
    LASSERT(a, a->count == 1,
        "Function 'len' passed wrong number of arguments!");
    LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
        "Function 'len' passed a non-Q-Expression!");

    lval* q = lval_take(a, 0);

    int i;
    for(i = 0; q->count; lval_del(lval_pop(q, 0)), i++);

    lval_del(q);

    return lval_int(i);
}

lval* builtin_cons(lval* a) {
    LASSERT(a, a->count == 2,
        "Function 'cons' passed wrong number of arguments!");
    LASSERT(a, a->cell[1]->type == LVAL_QEXPR,
        "Function 'cons' passed a non-Q-Expression!");

    lval* item = lval_pop(a, 0);
    lval* qexpr = lval_take(a, 0);
    lval_push(qexpr, item);

    return qexpr;
}

lval* builtin_head(lval* a) {
    /* Check Error Conditions */
    LASSERT(a, a->count == 1,
        "Function 'head' passed wrong number of arguments!");
    LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
        "Function 'head' passed a non-Q-Expression!");
    LASSERT(a, a->cell[0]->count != 0,
        "Function head passed {}!");

    /* Otherwise take the first argument */
    lval* v = lval_take(a, 0);

    /* Delete all elements that are not head and return */
    while(v->count > 1)
        lval_del(lval_pop(v, 1)); //Why on earth don't we just use lval_take again?

    return v;
}

lval* builtin_tail(lval* a) {
    /* Check error conditions */
    LASSERT(a, a->count == 1,
        "Function 'tail' passed wrong number of arguments!");
    LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
        "Function 'tail' passed a non-Q-Expression!");
    LASSERT(a, a->cell[0]->count != 0,
        "Function 'tail' passed {}!");

    /* Take first argument */
    lval* v = lval_take(a, 0);

    /* Delete first element and return */
    lval_del(lval_pop(v, 0));
    return v;
}

lval* builtin_list(lval* a) {
    a->type = LVAL_QEXPR;
    return a;
}

lval* builtin_eval(lval* a) {
    LASSERT(a, a->count == 1,
        "Function 'eval' passed wrong number of arguments!");
    LASSERT(a, a->cell[0]->type == LVAL_QEXPR,
        "Function 'eval' passed a non-Q-Expression!");

    lval* x = lval_take(a, 0);
    x->type = LVAL_SEXPR;
    return lval_eval(x);
}

lval* lval_join(lval* x, lval* y) {

    /* For each cell in 'y' add it to 'x' */
    while(y->count)
        x = lval_add(x, lval_pop(y, 0));

    /* Delete the empty 'y' and return 'x' */
    lval_del(y);
    return x;
}

lval* builtin_join(lval* a) {

    for(int i = 0; i < a->count; i++)
        LASSERT(a, a->cell[i]->type == LVAL_QEXPR,
            "Function 'join' passed a non-Q-Expression!");

    lval* x = lval_pop(a, 0);

    while(a->count)
        x = lval_join(x, lval_pop(a, 0));

    lval_del(a);
    return x;
}

/* Use operator string to see which operation to perform */
lval* eval_op(lval* x, char* op, lval* y, int arg_count) {

    /* Return the first error if we were passed any */
    if(x->type == LVAL_ERR) { lval_del(y); return x; }
    if(y && y->type == LVAL_ERR) { lval_del(x); return y; }

    /* If either of the arguments are floats, we'll evaluate them both as such */
    if(x->type == LVAL_FLOAT || (arg_count == 2 && y->type == LVAL_FLOAT))
        return eval_op_float(lval_to_float(x), op, lval_to_float(y), arg_count);

    return eval_op_int(x, op, y, arg_count);
}

lval* builtin_op(lval* a, char* op) {

    /* if not a number, return an error */
    for(int i = 0; i < a->count; i++) {
        if(a->cell[i]->type != LVAL_INT && a->cell[i]->type != LVAL_FLOAT) {
            lval_del(a);
            return lval_err("Cannot operate on a non-number!");
        }
    }

    /* We store the first arg in 'x' */
    lval* x = lval_pop(a, 0);

    /* Iterate the remaining children */
    int i;
    for(i = 0; a->count > 0; i++)
        if((x = eval_op(x, op, lval_pop(a, 0), 2))->type == LVAL_ERR)
            break;

    /* Handle single-argument cases */
    if(i == 0 && x->type != LVAL_ERR)
        x = eval_op(x, op, NULL, 1);

    lval_del(a);
    return x;
}

lval* builtin(lval* a, char* func) {
    if(strcmp("list", func) == 0) return builtin_list(a);
    if(strcmp("head", func) == 0) return builtin_head(a);
    if(strcmp("tail", func) == 0) return builtin_tail(a);
    if(strcmp("join", func) == 0) return builtin_join(a);
    if(strcmp("eval", func) == 0) return builtin_eval(a);
    if(strcmp("cons", func) == 0) return builtin_cons(a);
    if(strcmp("len", func) == 0) return builtin_len(a);
    if(strstr("+-/*%^", func) || strcmp("max", func) == 0 || strcmp("min", func) == 0) return builtin_op(a, func);
    lval_del(a);
    return lval_err("Unknown function!");
}

lval* lval_eval_sexpr(lval* v) {

    /* Evaluate children */
    for(int i = 0; i < v->count; i++)
        v->cell[i] = lval_eval(v->cell[i]);

    /* Error checking */
    for(int i = 0; i < v->count; i++)
        if(v->cell[i]->type == LVAL_ERR) return lval_take(v, i);

    /* Empty expressions */
    if(v->count == 0) return v;

    /* Single expression */
    if(v->count == 1) return lval_take(v, 0);

    /* Ensure first element is symbol */
    lval* f = lval_pop(v, 0);
    if(f->type != LVAL_SYM) {
        lval_del(f);
        lval_del(v);
        return lval_err("S-expression does not start with symbol!");
    }

    /* Call builtin with operator */
    lval* result = builtin(v, f->sym);
    lval_del(f);
    return result;
}

lval* lval_eval(lval* v) {
    /* Evaluate Sexpressions */
    if(v->type == LVAL_SEXPR) return lval_eval_sexpr(v);
    /* All other lval types remain the same */
    return v;
}

int main(int argc, char** argv) {

    /* Create Some Parsers */
    mpc_parser_t* Number   = mpc_new("number");
    mpc_parser_t* Integer  = mpc_new("integer");
    mpc_parser_t* Float    = mpc_new("float");
    mpc_parser_t* Symbol   = mpc_new("symbol");
    mpc_parser_t* Qexpr    = mpc_new("qexpr");
    mpc_parser_t* Sexpr    = mpc_new("sexpr");
    mpc_parser_t* Expr     = mpc_new("expr");
    mpc_parser_t* Lispy    = mpc_new("lispy");

    /* Define them with the following language */
    mpca_lang(MPCA_LANG_DEFAULT,
        "                                                                     \
            float   : /-?[0-9]+[.][0-9]+/ ;                                   \
            integer : /-?[0-9]+/ ;                                            \
            number  : <float> | <integer> ;                                   \
            symbol  : '+' | '-' | '*' | '/' | '%' | '^' | \"min\" | \"max\"   \
                    | \"list\" | \"head\" | \"tail\" | \"join\" | \"eval\"    \
                    | \"cons\" | \"len\" ;                                    \
            qexpr   : '{' <expr>* '}' ;                                       \
            sexpr   : '(' <expr>* ')' ;                                       \
            expr    : <number> | <symbol> | <sexpr> | <qexpr>;                \
            lispy   : /^/ <expr>* /$/ ;                                       \
        ",
        Float, Integer, Number, Symbol, Qexpr, Sexpr, Expr, Lispy);

    /* Print Version and Exit Information */
    puts("Lispy Version 0.0.0.0.2");
    puts("Press Ctrl+C to Exit");
    puts("(Quit command not yet implemented)\n");

    /* In a never ending loop */
    while(1) {

        /* Output our prompt and get input */
        char* input = readline("eval> ");

        /* Add input to history */
        add_history(input);

        /* Attempt to parse the user input */
        mpc_result_t r;
        if(mpc_parse("<stdin>", input, Lispy, &r)) {
            lval* result = lval_eval(lval_read(r.output));
            lval_println(result);
            lval_del(result);
            mpc_ast_delete(r.output);
        } else {
            /* Otherwise print the error */
            mpc_err_print(r.error);
            mpc_err_delete(r.error);
        }

        /* Free retrieved input */
        free(input);
    }

    mpc_cleanup(8, Float, Integer, Number, Symbol, Qexpr, Sexpr, Expr, Lispy);
    return 0;
}
