
# -----------------------------------------------------------------------------
# Calculator AST
#
# A simple calculator with variables -- Parser
# -----------------------------------------------------------------------------

import ply.yacc as yacc
import ply.lex as lex

import lexer as lexrules
from lexer import tokens

import sys

# Parsing rules

precedence = (
    ('nonassoc','PARALLEL','KERNELS'),
    ('nonassoc','LOOP'),
    )

######################## BEGIN AUXILIARIES PRODUCTIONS ########################

def p_program(t):
    '''program : lines'''
    t[0] = t[1]
    #print(t[0])
    print(" ".join(t[0]).replace('\n ', '\n').replace('  ', ' '))

#def p_structured_block_list(t):
#    '''structured_block_list : structured_block structured_block_list
#                             | '''
#    if len(t) == 3:
#        t[0] = t[1] + t[2]
#    else:
#        t[0] = []

def p_lines(t):
    '''lines : ignored_line lines
             | pragma lines
             | '''
    if len(t) == 3:
        t[0] = t[1] + t[2]
    else:
        t[0] = []

def p_ignored_line(t):
    'ignored_line : anything NEWLINE'
    t[0] = t[1] + ['\n']

def p_anything(t):
    '''anything : OTHER anything
                | ACC anything
                | PARALLEL anything
                | KERNELS anything
                | LOOP anything
                | NUM_WORKERS anything
                | VECTOR anything
                | COLLAPSE anything
                | REDUCTION anything
                | INDEPENDENT anything
                | COPY anything
                | COPYIN anything
                | COPYOUT anything
                | DATA anything
                | CREATE anything
                | INT anything
                | ID anything
                | LPAREN anything
                | RPAREN anything
                | RBRACKET anything
                | LBRACKET anything
                | BACKSLASH anything
                | SPACE anything
                | TAB anything
                | SUM anything
                | MUL anything
                | MAX anything
                | MIN anything
                | BITWISE_AND anything
                | BITWISE_OR anything
                | AND anything
                | OR anything
                | MODULE anything
                | COLON anything
                | COMMA anything
                | '''
    if len(t) == 3:
        t[0] = [t[1]] + t[2]
    else:
        t[0] = []

def p_pragma(t):
    'pragma : spaces BPRAGMA ACC construct EPRAGMA'
    t[0] = t[1] + ['#pragma omp'] + t[4] + ['\n']

def p_spaces(t):
    '''spaces : SPACE spaces
              | '''
    if len(t) == 3:
        t[0] = [' '] + t[2]
    else:
        t[0] = []

# Return: List of strings, each string is a clause
def p_clause_list(t):
    '''clause_list : clause
                   | clause_list clause'''
    t[0] = []
    if len(t) >= 2:
        t[0] += t[1]
    if len(t) >= 3:
        t[0] += t[2]

# Return: List of strings, each string is a variable
def p_var_list(t):
    '''var_list : ID
                | var_list COMMA ID'''
    if len(t) == 2:
        t[0] = [t[1]]
    if len(t) == 4:
        t[0] = t[1] + [t[3]]

# Return: String that represent the value
def p_value(t):
    '''value : ID 
             | INT'''
    t[0] = t[1]

# Return: String that represent the subarray
def p_subarray(t):
    '''subarray : ID LBRACKET value COLON value RBRACKET'''
    t[0] = t[1] + '[' + t[3] + ':' + t[5] + ']'

def p_data_var(t):
    '''data_var : subarray
                | ID'''
    t[0] = t[1]

# Return: List of strings, each string is a variable or a subarray
def p_data_var_list(t):
    '''data_var_list : data_var
                     | data_var_list COMMA data_var'''
    if len(t) == 2:
        t[0] = [t[1]]
    if len(t) == 4:
        t[0] = t[1] + [t[3]]

############################# BEGIN CONTRUCTIONS #############################

def p_construct_parallel_loop(t):
    '''construct : PARALLEL LOOP clause_list
                 | PARALLEL LOOP '''
    if len(t) == 4:
        t[0] = ['parallel for'] + t[3]
    else:
        t[0] = ['parallel for']


def p_construct_parallel(t):
    '''construct : PARALLEL clause_list
                 | PARALLEL ''' 
    if len(t) == 3:
        t[0] = ['parallel'] + t[2]
    else:
        t[0] = ['parallel']

def p_construct_loop(t):
    '''construct : LOOP clause_list
                 | LOOP '''
    if len(t) >= 3:
        t[0] = ['for'] + t[2]
    else:
        t[0] = ['for']

def p_construct_data(t):
    '''construct : DATA clause_list''' 
    t[0] = ['target data'] + t[2]

#def p_construct_kernels(t):
#    'construct : KERNELS clause_list'
#    t[0] = ['BLABLABLA'] + t[2]

################################ BEGIN CLAUSES ################################

# Return: List with a single string
def p_clause_num_workers(t):
    'clause : NUM_WORKERS LPAREN INT RPAREN'
    t[0] = ['num_threads(' + t[3] + ')']

# Return: List with a single string
def p_clause_vector(t):
    '''clause : VECTOR LPAREN INT RPAREN
              | VECTOR'''
    if len(t) == 5:
        t[0] = ['simd simdlen(' + t[3] + ')']
    else:
        t[0] = ['simd']

# Return: List with a single string
def p_clause_collapse(t):
    '''clause : COLLAPSE LPAREN INT RPAREN'''
    t[0] = ['collapse(' + t[3] + ')']

# Return: Empty list
def p_clause_independent(t):
    'clause : INDEPENDENT'
    t[0] = []

# Return: List with a single string
def p_clause_reduction(t):
    '''clause : REDUCTION LPAREN SUM COLON var_list RPAREN
              | REDUCTION LPAREN MUL COLON var_list RPAREN
              | REDUCTION LPAREN MAX COLON var_list RPAREN
              | REDUCTION LPAREN MIN COLON var_list RPAREN
              | REDUCTION LPAREN BITWISE_AND COLON var_list RPAREN
              | REDUCTION LPAREN BITWISE_OR COLON var_list RPAREN
              | REDUCTION LPAREN AND COLON var_list RPAREN
              | REDUCTION LPAREN OR COLON var_list RPAREN
              | REDUCTION LPAREN MODULE COLON var_list RPAREN '''

    # Transform the var_list into a unique string
    var_list = ", ".join(t[5])
    t[0] = ['reduction('+ t[3] + ':' + var_list + ')']

# Return: List with a single string
def p_clause_copy(t):
    'clause : COPY LPAREN data_var_list RPAREN'
    data_var_list = ", ".join(t[3])
    t[0] = ['map(tofrom:' + data_var_list + ')']

# Return: List with a single string
def p_clause_copyin(t):
    'clause : COPYIN LPAREN data_var_list RPAREN'
    data_var_list = ", ".join(t[3])
    t[0] = ['map(to:' + data_var_list + ')']

# Return: List with a single string
def p_clause_copyout(t):
    'clause : COPYOUT LPAREN data_var_list RPAREN'
    data_var_list = ", ".join(t[3])
    t[0] = ['map(from:' + data_var_list + ')']

# Return: List with a single string
def p_clause_create(t):
    'clause : COPYOUT LPAREN data_var_list RPAREN'
    data_var_list = ", ".join(t[3])
    t[0] = ['map(alloc:' + data_var_list + ')']

def p_error(t):
    print("Syntax error at '%s'" % t.value)

#I_TARGET = 0
#I_PARALLEL_FOR_KERNELS = 3
#I_KERNELS = 4
#I_TEAMS = 5
#
## A list of matrix
## input_lines[i] -> a matrix that represents a line of the input
## input_lines[i][0] -> 
#input_lines = []

if __name__ == '__main__':

    lexer = lex.lex(module=lexrules)
    # Control variable to generate a token to end of prama (EPRAGMA)
    lexer.in_pragma = 0
    parser = yacc.yacc()

    # If no input file was passed
    if len(sys.argv) == 1:
        print("No input passed, entering the interactive mode")

        while True:
            try:
                s = input('calc > ')   # Use raw_input on Python 2
            except EOFError:
                print()
                break
            s = s + "\n"
            parser.parse(s)

    else:

        filename = sys.argv[1]
        with open(filename, 'r') as input_file:
            input_to_parse = input_file.read()
        print("Input:")
        print("---------------------------------------------------")
        print(input_to_parse)
        print("---------------------------------------------------")
        print("Result:")
        print("---------------------------------------------------")
        parser.parse(input_to_parse)
        print("---------------------------------------------------")
