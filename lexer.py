# -----------------------------------------------------------------------------
# OpenAcc pragmas to OpenMP
#
# A simple calculator with variables -- Lexer
# -----------------------------------------------------------------------------
    
import ply.lex as lex

import sys

# Tokens
reserved = {
        'acc'         : 'ACC',
        'scop'        : 'SCOP',
        'endscop'     : 'ENDSCOP',
        'parallel'    : 'PARALLEL',
        'kernels'     : 'KERNELS',
        'loop'        : 'LOOP',
        'num_workers' : 'NUM_WORKERS',
        'vector'      : 'VECTOR',
        'collapse'    : 'COLLAPSE',
        'reduction'   : 'REDUCTION',
        'independent' : 'INDEPENDENT',
        'copy'        : 'COPY',
        'copyin'      : 'COPYIN',
        'copyout'     : 'COPYOUT',
        'create'      : 'CREATE',
        'gang'        : 'GANG',
        'data'        : 'DATA'
}
tokens = [
# Shoudn`t be on anything rules
    'NEWLINE', 
    'BPRAGMA',
    'EPRAGMA',

    'INT',
    'ID',
    'LPAREN',
    'RPAREN',
#    'LBRACE',
#    'RBRACE',
    'RBRACKET',
    'LBRACKET',
    'BACKSLASH',
    'SPACE',
    'TAB',

# OPERATIORS
    'SUM',
    'MUL',
    'MAX',
    'MIN',
    'BITWISE_AND',
    'BITWISE_OR',
    'AND',
    'OR',

# PUNCTUATION
    'MODULE',
    'COLON',
    'COMMA',
    'OTHER'
    ] + list(reserved.values())

def t_INT(t):
    r'[0-9]+'
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value,'ID')    # Check for reserved words
    return t

def t_BPRAGMA(t):
    r'\#[ ]*pragma'
    t.value = t.value.replace(" ", "")
    t.lexer.in_pragma += 1
    return t

def t_NEWLINE(t):
    r'\n'
    t.lexer.lineno += t.value.count("\n")
    if t.lexer.in_pragma > 0:
        t.lexer.in_pragma -= 1
        if t.lexer.in_pragma == 0:
            t.type = 'EPRAGMA'
            return t
    else:
        return t

def t_BACKSLASH(t):
    r'\\'
    if t.lexer.in_pragma > 0:
        t.lexer.in_pragma += 1
    # Output the BACKSLASH token only if lexer not in a pragma
    else:
        return t

def t_SPACE(t):
    r'\s'
    # Output the SPACE token only if lexer not in a pragma
    if t.lexer.in_pragma == 0:
        return t

def t_TAB(t):
    r'\t'
    # Output the TAB token only if lexer not in a pragma
    if t.lexer.in_pragma == 0:
        return t

def t_SUM(t):
    r'\+'
    return t

def t_MUL(t):
    r'\*'
    return t

def t_MAX(t):
    r'max'
    return t

def t_MIN(t):
    r'min'
    return t

def t_AND(t):
    r'&&'
    return t

def t_OR(t):
    r'\|\|'
    return t

def t_BITWISE_AND(t):
    r'&'
    return t

def t_BITWISE_OR(t):
    r'\|'
    return t

def t_MODULE(t):
    r'%'
    return t

def t_LPAREN(t):
    r'\('
    return t

def t_RPAREN(t):
    r'\)'
    return t

#def t_LBRACE(t):
#    r'{'
#    return t
#
#def t_RBRACE(t):
#    r'}'
#    return t
#
def t_LBRACKET(t):
    r'\['
    return t

def t_RBRACKET(t):
    r'\]'
    return t

def t_COLON(t):
    r':'
    return t

def t_COMMA(t):
    r','
    return t

def t_OTHER(t):
    r'\S+'
    return t

# Ignored characters
#t_ignore = ""
    
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

if __name__ == '__main__':
    lexer = lex.lex()
    # Control variable to generate a token to end of prama (EPRAGMA)
    lexer.in_pragma = 0
    
    # If no input file was passed
    if len(sys.argv) == 1:
        print("No input passed, entering the interactive mode")

        while True:
            try:
                s = input('calc > ')
            except EOFError:
                print()
                break
            s = s + "\n"
            lexer.input(s)
            for token in lexer:
                print(token)

    else:

        filename = sys.argv[1]
        with open(filename, 'r') as input_file:
            input_to_lex = input_file.read()
        print("Input:")
        print("---------------------------------------------------")
        print(input_to_lex)
        print("---------------------------------------------------")
        lexer.input(input_to_lex)
        for token in lexer:
            print(token)
