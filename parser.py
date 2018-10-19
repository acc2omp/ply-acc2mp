
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
import os

# Parsing rules

precedence = (
    ('nonassoc','PARALLEL','KERNELS'),
    ('nonassoc','LOOP'),
    )

def p_program(t):
    '''program : lines'''
    t[0] = t[1]
    t[0] =  "".join(t[0])

#def p_structured_block_list(t):
#    '''structured_block_list : structured_block structured_block_list
#                             | '''
#    if len(t) == 3:
#        t[0] = t[1] + t[2]
#    else:
#        t[0] = []

# Return: List of string, each string is a line
def p_lines_praga(t):
    '''lines : pragma lines'''
    t[0] = [" ".join(t[1]).replace("  ", " ").replace("\n ", "\n")] + t[2]

# Return: List of string, each string is a line
def p_lines_ignored_line(t):
    '''lines : ignored_line lines
             | '''
    if len(t) == 3:
        t[0] = ["".join(t[1])] + t[2]
    else:
        t[0] = []

def p_ignored_line(t):
    'ignored_line : anything NEWLINE'
    t[0] = t[1] + ['\n']

def p_anything(t):
    '''anything : OTHER anything
                | INT anything
                | word anything
                | other_mark anything
                | whitespace anything
                | operator anything
                | ponctuation anything
                | '''
    if len(t) == 3:
        t[0] = [t[1]] + t[2]
    else:
        t[0] = []

def p_pragma(t):
    'pragma : spaces BPRAGMA ACC construct EPRAGMA'
    t[0] = []
    for construct in t[4]:
        t[0] += t[1] + ['#pragma omp'] + construct + ['\n']
    #print("Pragma 1")

def p_pragma_scop(t):
    'pragma : spaces BPRAGMA SCOP EPRAGMA'
    t[0] = t[1] + ['#pragma scop'] + ['\n']
    #print("Pragma 2")

def p_pragma_endscop(t):
    'pragma : spaces BPRAGMA ENDSCOP EPRAGMA'
    t[0] = t[1] + ['#pragma endscop'] + ['\n']
    #print("Pragma 3")

######################## BEGIN AUXILIARIES PRODUCTIONS ########################

def p_spaces(t):
    '''spaces : whitespace spaces
              | '''
    if len(t) == 3:
        t[0] = [' '] + t[2]
    else:
        t[0] = []

def p_whitespace(t):
    '''whitespace : SPACE
                  | TAB'''
    if len(t) == 2:
        t[0] = t[1]

def p_word(t):
    '''word : ACC 
            | SCOP 
            | ENDSCOP 
            | PARALLEL 
            | KERNELS 
            | LOOP 
            | NUM_WORKERS 
            | VECTOR 
            | COLLAPSE 
            | REDUCTION 
            | INDEPENDENT 
            | COPY 
            | COPYIN 
            | COPYOUT 
            | CREATE 
            | GANG 
            | DATA 
            | ID'''
    if len(t) == 2:
        t[0] = t[1]

def p_operator(t):
    '''operator : SUM 
                | MUL 
                | MAX 
                | MIN 
                | BITWISE_AND 
                | BITWISE_OR 
                | AND 
                | OR 
                | MODULE'''
    if len(t) == 2:
        t[0] = t[1]

def p_ponctuation(t):
    '''ponctuation : COLON 
                   | COMMA'''
    if len(t) == 2:
        t[0] = t[1]

def p_other_mark(t):
    '''other_mark : LPAREN 
                  | RPAREN 
                  | RBRACKET 
                  | LBRACKET 
                  | BACKSLASH'''
    if len(t) == 2:
        t[0] = t[1]

def p_between_other_mark(t):
    '''between_other_marks : OTHER between_other_marks
                           | INT between_other_marks
                           | word between_other_marks
                           | whitespace between_other_marks
                           | operator between_other_marks
                           | ponctuation between_other_marks
                           | '''
    if len(t) == 3:
        t[0] = [t[1]] + t[2]
    if len(t) == 1:
        t[0] = []

def p_possible_comma(t):
    '''possible_comma : COMMA
                      | '''
    if len(t) == 2:
        t[0] = [',']
    else:
        t[0] = []

def p_varname(t):
    '''var_name : ID
                | ACC
                | SCOP
                | ENDSCOP
                | PARALLEL
                | KERNELS
                | LOOP
                | NUM_WORKERS
                | VECTOR
                | COLLAPSE
                | REDUCTION
                | INDEPENDENT
                | COPY
                | COPYIN
                | COPYOUT
                | CREATE
                | GANG
                | DATA'''
    t[0] = t[1]

# Return: Tuple, first element is a list of strings, each string is a clause and
# second element is a dictionarie with where the key 'clause_name' get the 
# clause parameter
def p_clause_list(t):
    '''clause_list : clause
                   | clause_list possible_comma clause'''
    if len(t) == 2:
        t[0] = t[1]
    if len(t) == 4:
        new_dict = t[1][1].copy()
        new_dict.update(t[3][1])
        t[0] = (t[1][0] + t[3][0], new_dict)

# Return: List of strings, each string is a variable
def p_var_list(t):
    '''var_list : var_name
                | var_list COMMA var_name'''
    if len(t) == 2:
        t[0] = [t[1]]
    if len(t) == 4:
        t[0] = t[1] + [t[3]]

# Return: String that represent the value
def p_value(t):
    '''value : var_name 
             | INT'''
    t[0] = t[1]

# Return: String that represent the subarray
def p_subarray(t):
    '''subarray : var_name LBRACKET between_other_marks RBRACKET
                | subarray LBRACKET between_other_marks RBRACKET'''
    t[0] = t[1] + '[' + "".join(t[3]) + ']'

def p_data_var(t):
    '''data_var : subarray
                | var_name'''
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

# Return: List of lists. Each list represents a construct and is a list of 
# strings
def p_construct_parallel_loop(t):
    '''construct : PARALLEL LOOP clause_list
                 | PARALLEL LOOP '''
    if len(t) == 4:
        t[0] = [['parallel for'] + t[3][0]]
        has_copy = False
        data_clauses = []
        if 'copy' in t[3][1].keys():
            data_clauses += ['map(tofrom:' + t[3][1]['copy'] + ')']
            has_copy = True
        if 'copyin' in t[3][1].keys():
            data_clauses += ['map(to:' + t[3][1]['copyin'] + ')']
            has_copy = True
        if 'copyout' in t[3][1].keys():
            data_clauses += ['map(from:' + t[3][1]['copyout'] + ')']
            has_copy = True
        if 'create' in t[3][1].keys():
            data_clauses += ['map(alloc:' + t[3][1]['create'] + ')']
            has_copy = True
        if has_copy:
            t[0] = [ ['target data'] + data_clauses ] + t[0]

    else:
        t[0] = [['parallel for']]


# Return: List of lists. Each list represents a construct and is a list of 
# strings
def p_construct_parallel(t):
    '''construct : PARALLEL clause_list
                 | PARALLEL ''' 
    if len(t) == 3:
        t[0] = [['parallel'] + t[2][0]]
        has_copy = False
        data_clauses = []
        if 'copy' in t[2][1].keys():
            data_clauses += ['map(tofrom:' + t[2][1]['copy'] + ')']
            has_copy = True
        if 'copyin' in t[2][1].keys():
            data_clauses += ['map(to:' + t[2][1]['copyin'] + ')']
            has_copy = True
        if 'copyout' in t[2][1].keys():
            data_clauses += ['map(from:' + t[2][1]['copyout'] + ')']
            has_copy = True
        if 'create' in t[2][1].keys():
            data_clauses += ['map(alloc:' + t[2][1]['create'] + ')']
            has_copy = True
        if has_copy:
            t[0] = [ ['target data'] + data_clauses ] + t[0]
    else:
        t[0] = [['parallel']]

# Return: List of lists. Each list represents a construct and is a list of 
# strings
def p_construct_loop(t):
    '''construct : LOOP clause_list
                 | LOOP '''
    if len(t) >= 3:
        t[0] = [['for'] + t[2][0]]
        if 'gang' in t[2][1].keys():
            t[0] = [ ['teams'] ] + t[0]
    else:
        t[0] = [['for']]

# Return: List of lists. Each list represents a construct and is a list of 
# strings
def p_construct_data(t):
    '''construct : DATA clause_list
                 | ''' 
    if len(t) == 3:
        t[0] = [['target data'] + t[2][0]]
        if 'copy' in t[2][1].keys():
            t[0][0] += ['map(tofrom:' + t[2][1]['copy'] + ')']
        if 'copyin' in t[2][1].keys():
            t[0][0] += ['map(to:' + t[2][1]['copyin'] + ')']
        if 'copyout' in t[2][1].keys():
            t[0][0] += ['map(from:' + t[2][1]['copyout'] + ')']
        if 'create' in t[2][1].keys():
            t[0][0] += ['map(alloc:' + t[2][1]['create'] + ')']
    else:
        t[0] = [['target data']]

################################ BEGIN CLAUSES ################################

# Return: Tuple, first element is a list with a single string and the second 
# element is a dictionary where the key 'clause_name' get the clause parameter
def p_clause_num_workers(t):
    'clause : NUM_WORKERS LPAREN INT RPAREN'
    t[0] = ( ['num_threads(' + t[3] + ')'], {'num_workers':t[3]} )

# Return: Tuple, first element is a list with a single string and the second 
# element is a dictionary where the key 'clause_name' get the clause parameter
def p_clause_vector(t):
    '''clause : VECTOR LPAREN INT RPAREN
              | VECTOR'''
    if len(t) == 5:
        t[0] = ( ['simd simdlen(' + t[3] + ')'], {'vector':t[3]} )
    else:
        t[0] = ( ['simd'], {'vector':None} )

# Return: Tuple, first element is a list with a single string and the second 
# element is a dictionary where the key 'clause_name' get the clause parameter
def p_clause_collapse(t):
    '''clause : COLLAPSE LPAREN INT RPAREN'''
    t[0] = ( ['collapse(' + t[3] + ')'], {'collapse':t[3]} )

# Return: Tuple, first element is a empty list and the second element is a 
# dictionary where the key 'clause_name' get the clause parameter
def p_clause_independent(t):
    'clause : INDEPENDENT'
    t[0] = ( [], {'independent':None} )

# Return: Tuple, first element is a list with a single string and the second 
# element is a dictionary where the key 'clause_name' get the clause parameter
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
    t[0] = ( ['reduction('+ t[3] + ':' + var_list + ')'], \
            {'reduction':(t[3], var_list)} )

# Return: Tuple, first element is a empty list and the second element is a 
# dictionary where the key 'clause_name' get the clause parameter
def p_clause_copy(t):
    'clause : COPY LPAREN data_var_list RPAREN'
    data_var_list = ", ".join(t[3])
    #t[0] = ['map(tofrom:' + data_var_list + ')']
    t[0] = ( [], {'copy':data_var_list} )

# Return: Tuple, first element is a empty list and the second element is a 
# dictionary where the key 'clause_name' get the clause parameter
def p_clause_copyin(t):
    'clause : COPYIN LPAREN data_var_list RPAREN'
    data_var_list = ", ".join(t[3])
    #t[0] = ['map(to:' + data_var_list + ')']
    t[0] = ( [], {'copyin':data_var_list} )

# Return: Tuple, first element is a empty list and the second element is a 
# dictionary where the key 'clause_name' get the clause parameter
def p_clause_copyout(t):
    'clause : COPYOUT LPAREN data_var_list RPAREN'
    data_var_list = ", ".join(t[3])
    #t[0] = ['map(from:' + data_var_list + ')']
    t[0] = ( [], {'copyout':data_var_list} )

# Return: Tuple, first element is a empty list and the second element is a 
# dictionary where the key 'clause_name' get the clause parameter
def p_clause_create(t):
    'clause : CREATE LPAREN data_var_list RPAREN'
    data_var_list = ", ".join(t[3])
    #t[0] = ['map(alloc:' + data_var_list + ')']
    t[0] = ( [], {'create':data_var_list} )

# Return: Tuple, first element is a empty list and the second element is a 
# dictionary where the key 'clause_name' get the clause parameter
def p_clause_gang(t):
    'clause : GANG'
    t[0] = ( [], {'gang':None} )

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
            result = parser.parse(s)
            print(result)

    else:

        filename = sys.argv[1]
        with open(filename, 'r') as input_file:
            input_to_parse = input_file.read()

        if len(sys.argv) == 2:
            print("Input:")
            print("---------------------------------------------------")
            print(input_to_parse)
            print("---------------------------------------------------")
            print("Result:")
            print("---------------------------------------------------")
            result = parser.parse(input_to_parse)
            print(result)
            print("---------------------------------------------------")

        else:
            result = parser.parse(input_to_parse)
            filename = sys.argv[2]
            with open(filename, 'w') as output_file:
                output_file.write(result)
            os.chmod(filename, 0o666)
