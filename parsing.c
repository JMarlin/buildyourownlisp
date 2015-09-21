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

#define LASSERT(args, cond, fmt, ...) \
    if(!(cond)) { \
        lval* err = lval_err(fmt, ##__VA_ARGS__); \
        lval_del(args); \
        return err; \
    }

#define LASSERT_NONEMPTY(fname, args, index) \
    if((args)->cell[(index)]->count == 0) { \
        lval* err = lval_err( \
            "Function '%s' passed an empty list in argument %i.", \
            (fname), (index) + 1); \
        lval_del(args); \
        return err; \
    }

#define LASSERT_TYPE(fname, args, index, tval) \
    if((args)->cell[(index)]->type != (tval)) { \
        lval* err = lval_err( \
            "Function '%s' passed wrong type in argument %i. " \
            "Got %s, expected %s.", \
            (fname), (index) + 1, ltype_name((args)->cell[(index)]->type), ltype_name(tval)); \
        lval_del(args); \
        return err; \
    }


#define LASSERT_NUM(fname, args, cnt) \
    if((args)->count != (cnt)) { \
        lval* err = lval_err( \
            "Function '%s' passed wrong number of arguments. " \
            "Got %i items, but function takes %i.", \
            (fname), (args)->count, (cnt)); \
        lval_del(args); \
        return err; \
    }

/* Forward declarations */
struct lval;
struct lenv;
typedef struct lval lval;
typedef struct lenv lenv;

/* Create Enumeration of Possible lval types */
enum { LVAL_INT, LVAL_FLOAT, LVAL_ERR, LVAL_SYM, LVAL_FUN, LVAL_SEXPR, LVAL_QEXPR, LVAL_EXIT };

typedef lval*(*lbuiltin)(lenv*, lval*);

/* Declare lval struct */
struct lval {
    int type;

    /* Basic */
    long inum;
    double fnum;
    char* err;
    char* sym;

    /* Function */
    lbuiltin builtin;
    lenv* env;
    lval* formals;
    lval* body;

    /* Expression */
    int count;
    struct lval** cell;
};

struct lenv {
    lenv* par;
    int count;
    char** syms;
    lval** vals;
};

lval* lval_exit() {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_EXIT;
    return v;
}

lval* lval_fun(lbuiltin func) {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_FUN;
    v->builtin = func;
    return v;
}

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
lval* lval_err(char* fmt, ...) {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_ERR;

    /* Create a va list and initialize it */
    va_list va;
    va_start(va, fmt);

    /* Allocate 512 bytes of space */
    v->err = malloc(512);

    /* printf the error string with a maximum of 511 characters */
    vsnprintf(v->err, 511, fmt, va);

    /* Reallocate to number of bytes actually used */
    v->err = realloc(v->err, strlen(v->err) + 1);

    /* Cleanup our va list */
    va_end(va);

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

char* ltype_name(int t) {
    switch(t) {
        case LVAL_EXIT: return "Exit Signal";
        case LVAL_FUN: return "Function";
        case LVAL_INT: return "Integer";
        case LVAL_FLOAT: return "Float";
        case LVAL_ERR: return "Error";
        case LVAL_SYM: return "Symbol";
        case LVAL_SEXPR: return "S-Expression";
        case LVAL_QEXPR: return "Q-Expression";
        default: return "Unknown";
    }
}

void lenv_del(lenv* e);

void lval_del(lval* v) {

    switch(v->type) {
        /* Do nothing special for number types */
        case LVAL_FLOAT:
        case LVAL_INT:
        case LVAL_EXIT:
            break;

        case LVAL_FUN:
            if(!v->builtin) {
                lenv_del(v->env);
                lval_del(v->formals);
                lval_del(v->body);
            }
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

lenv* lenv_copy(lenv* e);

lval* lval_copy(lval*v ) {

    lval* x = malloc(sizeof(lval));
    x->type = v->type;

    switch(v->type) {

        case LVAL_FUN:
            if(v->builtin) {
                x->builtin = v->builtin;
            } else {
                x->builtin = NULL;
                x->env = lenv_copy(v->env);
                x->formals = lval_copy(v->formals);
                x->body = lval_copy(v->body);
            }
        break;

        /* Copy functions and numbers directly */
        case LVAL_FLOAT: x->fnum = v->fnum; break;
        case LVAL_INT: x->inum = v->inum; break;

        /* Copy strings using malloc and strcpy */
        case LVAL_ERR:
            x->err = malloc(strlen(v->err) + 1);
            strcpy(x->err, v->err);
        break;

        case LVAL_SYM:
            x->sym = malloc(strlen(v->sym) + 1);
            strcpy(x->sym, v->sym);
        break;

        /* Copy lists by copying each sub-expression */
        case LVAL_SEXPR:
        case LVAL_QEXPR:
            x->count = v->count;
            x->cell = malloc(sizeof(lval*) * x->count);

            for(int i = 0; i < x->count; i++)
                x->cell[i] = lval_copy(v->cell[i]);

        break;
    }

    return x;
}


lenv* lenv_copy(lenv* e) {

    lenv* n = malloc(sizeof(lenv));
    n->par = e->par;
    n->count = e->count;
    n->syms = malloc(sizeof(char*) * n->count);
    n->vals = malloc(sizeof(lval*) * n->count);

    for(int i = 0; i < e->count; i++) {
        n->syms[i] = malloc(strlen(e->syms[i]) + 1);
        strcpy(n->syms[i], e->syms[i]);
        n->vals[i] = lval_copy(e->vals[i]);
    }

    return n;
}


lenv* lenv_new(void) {
    lenv* e = malloc(sizeof(lenv));
    e->par = NULL;
    e->count = 0;
    e->syms = NULL;
    e->vals = NULL;
    return e;
}

void lenv_del(lenv* e) {
    for(int i = 0; i < e->count; i++) {
        free(e->syms[i]);
        lval_del(e->vals[i]);
    }
    free(e->syms);
    free(e->vals);
    free(e);
}

lval* lenv_get(lenv* e, lval* k) {

    /* Iterate over all items in environment */
    for(int i = 0; i < e->count; i++) {
        /* Check if the stored string matches the symbol string */
        /* If it does, return a copy of the value */
        if(strcmp(e->syms[i], k->sym) == 0) {
            return lval_copy(e->vals[i]);
        }
    }

    /* If not symbol found look in parent or return error */
    if(e->par) {
        return lenv_get(e->par, k);
    } else {
        return lval_err("Unbound symbol '%s'!", k->sym);
    }
}

//Reverse lookup the name of a builtin by its builtin pointer
char* lenv_get_fname(lenv* e, lbuiltin bp) {

    lenv* glob = e;

    /* Make sure we're looking in the global scope */
    while(e->par)
        e = e->par;

    /* Iterate over all items in environment */
    for(int i = 0; i < e->count; i++) {
        /* Check if the stored string matches the symbol string */
        /* If it does, return a copy of the value */
        if(e->vals[i]->builtin != NULL && e->vals[i]->builtin == bp) {
            return e->syms[i];
        }
    }

    /* If not pointer found return error */
    return "invalid builtin";
}

void lenv_put(lenv* e, lval* k, lval* v) {

    /* Iterate over all items in environment */
    /* This is to see if variable already exists */
    for(int i = 0; i < e->count; i++) {

        /* If variable is found delete item at that position */
        /* and replace with variable supplied by user */
        if(strcmp(e->syms[i], k->sym) == 0) {
            lval_del(e->vals[i]);
            e->vals[i] = lval_copy(v);
            return;
        }
    }

    /* If no existing exntry found allovate space for new entry */
    e->count++;
    e->vals = realloc(e->vals, sizeof(lval*) * e->count);
    e->syms = realloc(e->syms, sizeof(char*) * e->count);

    /* Copy contents of lval and symbol string into new location */
    e->vals[e->count - 1] = lval_copy(v);
    e->syms[e->count - 1] = malloc(strlen(k->sym) + 1);
    strcpy(e->syms[e->count - 1], k->sym);
}

void lenv_def(lenv* e, lval* k, lval* v) {

    /* Get the global scope */
    while(e->par)
        e = e->par;

    /* Put the value in e */
    lenv_put(e, k, v);
}

void lval_expr_print(lenv* e, lval* v, char open, char close);

void lval_print(lenv*e ,lval* v) {
    switch(v->type) {
        case LVAL_INT:   printf("%li", v->inum); break;
        case LVAL_FLOAT: printf("%f", v->fnum); break;
        case LVAL_ERR:   printf("Error: %s", v->err); break;
        case LVAL_EXIT:  printf("Exit Signal"); break;
        case LVAL_SYM:   printf(v->sym); break;
        case LVAL_SEXPR: lval_expr_print(e, v, '(', ')'); break;
        case LVAL_QEXPR: lval_expr_print(e, v, '{', '}'); break;
        case LVAL_FUN:
            if(v->builtin) {
                printf("<builtin %s>", lenv_get_fname(e, v->builtin));
            } else {
                printf("(\\ "); lval_print(e, v->formals);
                putchar(' '); lval_print(e, v->body); putchar(')');
            }
        break;
    }
}

void lval_expr_print(lenv* e, lval* v, char open, char close) {
    putchar(open);
    for(int i = 0; i < v->count; i++) {

        /* Print value contained within */
        lval_print(e, v->cell[i]);

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
void lval_println(lenv* e, lval* v) {
    lval_print(e, v);
    putchar('\n');
}

void lenv_add_builtin(lenv* e, char* name, lbuiltin func) {
    lval* k = lval_sym(name);
    lval* v = lval_fun(func);
    lenv_put(e, k ,v);
    lval_del(k);
    lval_del(v);
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
            if(strcmp(op, "<") == 0) ret_val = arg_count < 2 ? lval_err("Too few arguments") : (x->fnum < y->fnum ? x : lval_float(0));
            if(strcmp(op, ">") == 0) ret_val = arg_count < 2 ? lval_err("Too few arguments") : (x->fnum > y->fnum ? x : lval_float(0));
            if(strcmp(op, "==") == 0) ret_val = arg_count < 2 ? lval_err("Too few arguments") : (x->fnum == y->fnum ? x : lval_float(0));
            if(strcmp(op, "<=") == 0) ret_val = arg_count < 2 ? lval_err("Too few arguments") : (x->fnum <= y->fnum ? x : lval_float(0));
            if(strcmp(op, ">=") == 0) ret_val = arg_count < 2 ? lval_err("Too few arguments") : (x->fnum >= y->fnum ? x : lval_float(0));
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
        if(strcmp(op, "<") == 0) ret_val = arg_count < 2 ? lval_err("Too few arguments") : (x->inum < y->inum ? x : lval_int(0));
        if(strcmp(op, ">") == 0) ret_val = arg_count < 2 ? lval_err("Too few arguments") : (x->inum > y->inum ? x : lval_int(0));
        if(strcmp(op, "==") == 0) ret_val = arg_count < 2 ? lval_err("Too few arguments") : (x->inum == y->inum ? x : lval_int(0));
        if(strcmp(op, "<=") == 0) ret_val = arg_count < 2 ? lval_err("Too few arguments") : (x->inum <= y->inum ? x : lval_int(0));
        if(strcmp(op, ">=") == 0) ret_val = arg_count < 2 ? lval_err("Too few arguments") : (x->inum >= y->inum ? x : lval_int(0));
    }

    if(ret_val != x) lval_del(x);
    if(y && ret_val != y) lval_del(y);

    if(!ret_val)
        return lval_err("Invalid integer operation");
    else
        return ret_val;
}

lval* lval_eval(lenv* e, lval* v);

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

lval* lval_lambda(lval* formals, lval* body) {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_FUN;

    /* Set Builtin to Null */
    v->builtin = NULL;

    /* Build new environment */
    v->env = lenv_new();

    /* Set Formals and body */
    v->formals = formals;
    v->body = body;
    return v;
}

lval* builtin_eval(lenv* e, lval* a);
lval* builtin_list(lenv* e, lval* a);

lval* lval_call(lenv* e, lval* f, lval* a) {

    /* If builtin then just apply it */
    if(f->builtin)
        return f->builtin(e, a);

    /* Record argument counts */
    int given = a->count;
    int total = f->formals->count;

    /* While arguments still remain to be processed */
    while(a->count) {

        /* If we've run out of formal arguments to bind */
        if(f->formals->count == 0) {
            lval_del(a); return lval_err(
                "Function passed too many arguments. "
                "Got %i, expected %i.", given, total);
        }

        /* Pop the first symbol from the formals */
        lval* sym = lval_pop(f->formals, 0);

        /* Special case to deal with '&' */
        if(strcmp(sym->sym, "&") == 0) {

            /* Ensure '&' is followed by another symbol */
            if(f->formals->count != 1) {
                lval_del(a);
                return lval_err("Function format invalid. "
                    "Symbol '&' not followed by single symbol.");
            }

            /* Next formal should be bound to remaining arguments */
            lval* nsym = lval_pop(f->formals, 0);
            lenv_put(f->env, nsym, builtin_list(e, a));
            lval_del(sym);
            lval_del(nsym);
            break;
        }

        /* Pop the next argument from the list */
        lval* val = lval_pop(a, 0);

        /* Bind a copy into the function's environment */
        lenv_put(f->env, sym, val);

        /* Delete symbol and value */
        lval_del(sym);
        lval_del(val);
    }

    /* Argument list is now bound so can be cleaned up */
    lval_del(a);

    /* If an '&' remains in the formal list, bind it to an empty list */
    if(f->formals->count > 0 && strcmp(f->formals->cell[0]->sym, "&") == 0) {

        /* Check to ensure that & is not passed invalidly */
        if(f->formals->count != 2) {
            return lval_err("Function format invalid "
                "Symbol '&' not followed by single symbol.");
        }

        /* Pop and delete '&' symbol */
        lval_del(lval_pop(f->formals, 0));

        /* Pop next symbol and create empty list */
        lval* sym = lval_pop(f->formals, 0);
        lval* val = lval_qexpr();

        /* Bind to environment and delete */
        lenv_put(f->env, sym, val);
        lval_del(sym);
        lval_del(val);
    }

    /* If all formals have been bound, evaluate */
    if(f->formals->count == 0) {

        /* Set environment parent to evaluation environment */
        f->env->par = e;

        /* Evaluate and return */
        return builtin_eval(f->env, lval_add(lval_sexpr(), lval_copy(f->body)));
    } else {

        /* Otherwise, return partially evaluated function */
        return lval_copy(f);
    }
}

lval* builtin_lambda(lenv* e, lval* a) {

    /* Check two arguments, each of which are Q-expressions */
    LASSERT_NUM("\\", a, 2);
    LASSERT_TYPE("\\", a, 0, LVAL_QEXPR);
    LASSERT_TYPE("\\", a, 1, LVAL_QEXPR);

    /* Check first Q-Expression contains only symbols */
    for(int i = 0; i < a->cell[0]->count; i++) {

        LASSERT(a, (a->cell[0]->cell[i]->type == LVAL_SYM),
            "Cannot define non-symbol. Got %s, exprected %s.",
            ltype_name(a->cell[0]->cell[i]->type), ltype_name(LVAL_SYM));
    }

    /* Pop first two arguments and pass them to lval_lambda */
    lval* formals = lval_pop(a, 0);
    lval* body = lval_pop(a, 0);
    lval_del(a);

    return lval_lambda(formals, body);
}

lval* builtin_exit(lenv* e, lval* a) {

    LASSERT_NUM("exit", a, 0);

    lval_del(a);
    return lval_exit();
}

lval* builtin_init(lenv* e, lval* a) {
    LASSERT_NUM("init", a, 1);
    LASSERT_TYPE("init", a, 0, LVAL_QEXPR);
    LASSERT_NONEMPTY("init", a, 0);

    lval* q = lval_take(a, 0);
    lval_del(lval_pop(q, q->count - 1));

    return q;
}

lval* builtin_len(lenv* e, lval* a) {
    LASSERT_NUM("len", a, 1);
    LASSERT_TYPE("len", a, 0, LVAL_QEXPR);

    lval* q = lval_take(a, 0);

    int i = q->count;

    lval_del(q);

    return lval_int(i);
}

lval* builtin_cons(lenv* e, lval* a) {
    LASSERT_NUM("cons", a, 2);
    LASSERT_TYPE("cons", a, 1, LVAL_QEXPR);

    lval* item = lval_pop(a, 0);
    lval* qexpr = lval_take(a, 0);
    lval_push(qexpr, item);

    return qexpr;
}

lval* builtin_head(lenv* e, lval* a) {
    /* Check Error Conditions */
    LASSERT_NUM("head", a, 1);
    LASSERT_TYPE("head", a, 0, LVAL_QEXPR);
    LASSERT_NONEMPTY("head", a, 0);

    /* Otherwise take the first argument */
    lval* v = lval_take(a, 0);

    /* Delete all elements that are not head and return */
    while(v->count > 1)
        lval_del(lval_pop(v, 1)); //Why on earth don't we just use lval_take again?

    return v;
}

lval* builtin_tail(lenv* e, lval* a) {
    /* Check error conditions */
    LASSERT_NUM("tail", a, 1);
    LASSERT_TYPE("tail", a, 0, LVAL_QEXPR);
    LASSERT_NONEMPTY("head", a, 0);

    /* Take first argument */
    lval* v = lval_take(a, 0);

    /* Delete first element and return */
    lval_del(lval_pop(v, 0));
    return v;
}

lval* builtin_list(lenv* e, lval* a) {
    a->type = LVAL_QEXPR;
    return a;
}

lval* builtin_eval(lenv* e, lval* a) {
    LASSERT_NUM("eval", a, 1);
    LASSERT_TYPE("eval", a, 0, LVAL_QEXPR);

    lval* x = lval_take(a, 0);
    x->type = LVAL_SEXPR;
    return lval_eval(e, x);
}

lval* lval_join(lval* x, lval* y) {

    /* For each cell in 'y' add it to 'x' */
    while(y->count)
        x = lval_add(x, lval_pop(y, 0));

    /* Delete the empty 'y' and return 'x' */
    lval_del(y);
    return x;
}

lval* builtin_if(lenv* e, lval* a) {

    //Should be three args
    LASSERT_NUM("if", a, 3);

    //First arg should be an int or a float
    LASSERT(a, a->cell[0]->type == LVAL_INT || a->cell[1]->type == LVAL_FLOAT,
        "First argument to if must be a numeric expression. Found %s instead.", ltype_name(a->cell[0]->type));

    //Next two args should be q-expressions
    LASSERT_TYPE("if", a, 1, LVAL_QEXPR);
    LASSERT_TYPE("if", a, 2, LVAL_QEXPR);

    //Check to make sure the number is nonzero
    lval* eval_expr;
    lval* number = lval_pop(a, 0);
    if((number->type == LVAL_INT && number->inum != 0) ||
        (number->type == LVAL_FLOAT && number->fnum != 0.0)) {

        //If nonzero, get the first expression
        eval_expr = lval_pop(a, 0);
    } else {

        //If zero, get the second expression
        eval_expr = lval_pop(a, 1);
    }

    //Drop unused values
    lval_del(number);
    lval_del(a);

    //Evaluate the selected expression
    builtin_eval(e, lval_add(lval_sexpr(), eval_expr));
}

lval* builtin_join(lenv* e, lval* a) {

    LASSERT(a, a->count > 0, "Function join requires arguments.");

    for(int i = 0; i < a->count; i++) {

        LASSERT_TYPE("join", a, i, LVAL_QEXPR);
    }

    lval* x = lval_pop(a, 0);

    while(a->count)
        x = lval_join(x, lval_pop(a, 0));

    lval_del(a);
    return x;
}

lval* builtin_var(lenv* e, lval* a, char* func) {

    LASSERT(a, a->count > 0, "Function %s requires arguments.", func);
    LASSERT_TYPE(func, a, 0, LVAL_QEXPR);

    /* First argument is symbol list */
    lval* syms = a->cell[0];

    /* Ensure all elements of first list are symbols */
    for(int i = 0; i < syms->count; i++) {

        LASSERT(a, (syms->cell[i]->type == LVAL_SYM),
            "Function '%s' cannot define non-symbol. "
            "Got %s, expected %s.", func,
            ltype_name(syms->cell[i]->type),
            ltype_name(LVAL_SYM));
    }

    /* Check correct number of symbols and values */
    LASSERT(a, syms->count == a->count - 1,
        "Function 'def' argument count mismatch");

    /* Assign copies of values to symbols */
    for(int i = 0; i < syms->count; i++) {

        /* If 'def' define globally. If 'put', define locally */
        if(strcmp(func, "def") == 0)
            lenv_def(e, syms->cell[i], a->cell[i+1]);

        if(strcmp(func, "put") == 0)
            lenv_put(e, syms->cell[i], a->cell[i+1]);
    }

    lval_del(a);
    return lval_sexpr();
}

lval* builtin_def(lenv* e, lval* a) {
    return builtin_var(e, a, "def");
}

lval* builtin_put(lenv* e, lval* a) {
    return builtin_var(e, a, "put");
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

lval* builtin_op(lenv* e, lval* a, char* op) {

    LASSERT(a, a->count > 0, "Operator %s requires arguments.", op);

    /* if not a number, return an error */
    for(int i = 0; i < a->count; i++) {
        if(a->cell[i]->type != LVAL_INT && a->cell[i]->type != LVAL_FLOAT) {
            int t = a->cell[i]->type;
            lval_del(a);
            return lval_err("Operator passed incorrect type in argument %i. "
                "Got %s, Expected float or int.",
                i, ltype_name(t)
            );
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

lval* builtin_add(lenv* e, lval* a) {
    return builtin_op(e, a, "+");
}

lval* builtin_sub(lenv* e, lval* a) {
    return builtin_op(e, a, "-");
}

lval* builtin_mul(lenv* e, lval* a) {
    return builtin_op(e, a, "*");
}

lval* builtin_div(lenv* e, lval* a) {
    return builtin_op(e, a, "/");
}

lval* builtin_mod(lenv* e, lval* a) {
    return builtin_op(e, a, "%");
}

lval* builtin_pow(lenv* e, lval* a) {
    return builtin_op(e, a, "^");
}

lval* builtin_min(lenv* e, lval* a) {
    return builtin_op(e, a, "min");
}

lval* builtin_max(lenv* e, lval* a) {
    return builtin_op(e, a, "max");
}

lval* builtin_lt(lenv* e, lval* a) {
    return builtin_op(e, a, "<");
}

lval* builtin_gt(lenv* e, lval* a) {
    return builtin_op(e, a, ">");
}

lval* builtin_eq(lenv* e, lval* a) {
    return builtin_op(e, a, "==");
}

lval* builtin_gte(lenv* e, lval* a) {
    return builtin_op(e, a, ">=");
}

lval* builtin_lte(lenv* e, lval* a) {
    return builtin_op(e, a, "<=");
}

void lenv_add_builtins(lenv* e) {
    /* List functions */
    lenv_add_builtin(e, "list", builtin_list);
    lenv_add_builtin(e, "head", builtin_head);
    lenv_add_builtin(e, "tail", builtin_tail);
    lenv_add_builtin(e, "eval", builtin_eval);
    lenv_add_builtin(e, "join", builtin_join);
    lenv_add_builtin(e, "cons", builtin_cons);
    lenv_add_builtin(e, "len", builtin_len);
    lenv_add_builtin(e, "init", builtin_init);
    lenv_add_builtin(e, "def", builtin_def);
    lenv_add_builtin(e, "=", builtin_put);
    lenv_add_builtin(e, "exit", builtin_exit);
    lenv_add_builtin(e, "\\", builtin_lambda);
    lenv_add_builtin(e, "if", builtin_if);

    /* Math operations */
    lenv_add_builtin(e, "+", builtin_add);
    lenv_add_builtin(e, "-", builtin_sub);
    lenv_add_builtin(e, "*", builtin_mul);
    lenv_add_builtin(e, "/", builtin_div);
    lenv_add_builtin(e, "%", builtin_mod);
    lenv_add_builtin(e, "^", builtin_pow);
    lenv_add_builtin(e, "max", builtin_max);
    lenv_add_builtin(e, "min", builtin_min);
    lenv_add_builtin(e, "<", builtin_lt);
    lenv_add_builtin(e, ">", builtin_gt);
    lenv_add_builtin(e, "==", builtin_eq);
    lenv_add_builtin(e, ">=", builtin_gte);
    lenv_add_builtin(e, "<=", builtin_lte);
}

lval* lval_eval_sexpr(lenv* e, lval* v) {

    /* Evaluate children */
    for(int i = 0; i < v->count; i++)
        v->cell[i] = lval_eval(e, v->cell[i]);

    /* Error and exit checking */
    for(int i = 0; i < v->count; i++)
        if(v->cell[i]->type == LVAL_ERR || v->cell[i]->type == LVAL_EXIT) return lval_take(v, i);

    /* Empty expressions */
    if(v->count == 0) return v;

    /* Single expression */
    if(v->count == 1) {
        lval* val = lval_pop(v, 0);
        if(val->type == LVAL_FUN && val->builtin && val->builtin == builtin_exit)
            return lval_call(e, val, v);

        lval_del(v);
        return val;
    }

    /* Ensure first element is a function after evaluation */
    lval* f = lval_pop(v, 0);
    if(f->type != LVAL_FUN) {
        lval* err = lval_err(
            "S-Expression starts with an incorrect type. "
            "Got %s, expected %s.",
            ltype_name(f->type), ltype_name(LVAL_FUN));
        lval_del(f);
        lval_del(v);
        return err;
    }

    /* Call builtin with operator */
    lval* result = lval_call(e, f, v);
    lval_del(f);
    return result;
}

lval* lval_eval(lenv* e, lval* v) {

    /* Evaluate Sexpressions */
    if(v->type == LVAL_SYM) {
        lval* x = lenv_get(e, v);
        lval_del(v);
        return x;
    }

    if(v->type == LVAL_SEXPR) return lval_eval_sexpr(e, v);

    /* All other lval types remain the same */
    return v;
}

int main(int argc, char** argv) {

    int running = 1;

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
            symbol  : /[a-zA-Z0-9_+\\-*\\/\\\\=<>!&]+/ ;                      \
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

    lenv* e = lenv_new();
    lenv_add_builtins(e);

    /* In a never ending loop */
    while(running) {

        /* Output our prompt and get input */
        char* input = readline("eval> ");

        /* Add input to history */
        add_history(input);

        /* Attempt to parse the user input */
        mpc_result_t r;
        if(mpc_parse("<stdin>", input, Lispy, &r)) {
            lval* result = lval_eval(e, lval_read(r.output));
            lval_println(e, result);
            if(result->type == LVAL_EXIT)
                running = 0;
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

    lenv_del(e);
    mpc_cleanup(8, Float, Integer, Number, Symbol, Qexpr, Sexpr, Expr, Lispy);
    return 0;
}
