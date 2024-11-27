import ply.lex as lex
import ply.yacc as yacc
import time
import os

# Token definitions
tokens = [
    'NUMAR', 'PLUS', 'MINUS', 'INMULTESTE', 'IMPARTE', 
    'PARANTEZA_STANGA', 'PARANTEZA_DREAPTA', 'ID', 'EGAL', 'AFISEAZA', 'DACA', 'ALTEL',
    'ACOLADA_STANGA', 'ACOLADA_DREAPTA', 'MARE', 'MIC', 'SI', 'SAU', 'INTRARE', 'STRING', 'WHILE'
]

# Token regex rules
t_PLUS = r'\+'
t_MINUS = r'-'
t_INMULTESTE = r'\*'
t_IMPARTE = r'/'
t_PARANTEZA_STANGA = r'\('
t_PARANTEZA_DREAPTA = r'\)'
t_EGAL = r'='
t_ACOLADA_STANGA = r'\{'
t_ACOLADA_DREAPTA = r'\}'
t_MARE = r'>'
t_MIC = r'<'
t_SI = r'si'
t_SAU = r'sau'
t_STRING = r'\".*?\"'
t_WHILE = r'cat'

t_ignore = ' \t'

# Keywords
def t_AFISEAZA(t):
    r'afiseaza|arata'
    return t

def t_DACA(t):
    r'daca'
    return t

def t_ALTEL(t):
    r'altfel'
    return t

def t_INTRARE(t):
    r'intrare'
    return t

def t_NUMAR(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_comment(t):
    r'\#.*'
    pass  # Ignore comments

def t_error(t):
    print(f"Caracter ilegal '{t.value[0]}'")
    t.lexer.skip(1)

lexer = lex.lex()

# Parsing rules
def p_program(p):
    '''program : statement
               | statement program'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[2]

def p_statement_assign(p):
    'statement : ID EGAL expression'
    p[0] = ('assign', p[1], p[3])

def p_statement_afiseaza(p):
    'statement : AFISEAZA expression'
    p[0] = ('afiseaza', p[2])

def p_statement_intrare(p):
    'statement : INTRARE ID'
    p[0] = ('intrare', p[2])

def p_statement_daca(p):
    '''statement : DACA expression block
                 | DACA expression block ALTEL block'''
    if len(p) == 4:
        p[0] = ('daca', p[2], p[3])
    else:
        p[0] = ('daca_altfel', p[2], p[3], p[5])

def p_statement_while(p):
    'statement : WHILE expression block'
    p[0] = ('while', p[2], p[3])

def p_block(p):
    '''block : ACOLADA_STANGA program ACOLADA_DREAPTA'''
    p[0] = p[2]

def p_expression_binop(p):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression INMULTESTE expression
                  | expression IMPARTE expression
                  | expression MARE expression
                  | expression MIC expression
                  | expression SI expression
                  | expression SAU expression'''
    p[0] = ('binop', p[2], p[1], p[3])

def p_expression_group(p):
    'expression : PARANTEZA_STANGA expression PARANTEZA_DREAPTA'
    p[0] = p[2]

def p_expression_numar(p):
    'expression : NUMAR'
    p[0] = p[1]

def p_expression_string(p):
    'expression : STRING'
    p[0] = p[1][1:-1]

def p_expression_id(p):
    'expression : ID'
    p[0] = ('var', p[1])

def p_error(p):
    print(f"Eroare de sintaxa la '{p.value}'")

parser = yacc.yacc()

# Interpreter functions
env = {}

def eval_expr(expr):
    if isinstance(expr, int) or isinstance(expr, str):
        return expr
    if expr[0] == 'binop':
        op = expr[1]
        left = eval_expr(expr[2])
        right = eval_expr(expr[3])
        if op == '+':
            return left + right
        elif op == '-':
            return left - right
        elif op == '*':
            return left * right
        elif op == '/':
            return left / right
        elif op == '>':
            return 1 if left > right else 0
        elif op == '<':
            return 1 if left < right else 0
        elif op == 'si':
            return 1 if left and right else 0
        elif op == 'sau':
            return 1 if left or right else 0
    elif expr[0] == 'var':
        return env.get(expr[1], 0)

def exec_stmt(stmt):
    if stmt[0] == 'assign':
        var_name = stmt[1]
        value = eval_expr(stmt[2])
        env[var_name] = value
    elif stmt[0] == 'afiseaza':
        value = eval_expr(stmt[1])
        print(value)
    elif stmt[0] == 'intrare':
        var_name = stmt[1]
        env[var_name] = int(input(f"Introdu valoarea pentru {var_name}: "))
    elif stmt[0] == 'daca':
        condition = eval_expr(stmt[1])
        if condition:
            exec_program(stmt[2])
    elif stmt[0] == 'daca_altfel':
        condition = eval_expr(stmt[1])
        if condition:
            exec_program(stmt[2])
        else:
            exec_program(stmt[3])
    elif stmt[0] == 'while':
        condition = eval_expr(stmt[1])
        while condition:
            exec_program(stmt[2])
            condition = eval_expr(stmt[1])

def exec_program(program):
    for stmt in program:
        exec_stmt(stmt)

def run_code_from_file(file_path):
    os.system('cls')
    with open(file_path, 'r') as file:
        code = file.read()
    lexer.input(code)
    while True:
        token = lexer.token()
        if not token:
            break
    
    result = parser.parse(code)
    if result is not None:
        exec_program(result)

# Example usage
file_name = input("Introduceți numele fișierului: ")
file_path = f"{file_name}.rspr"
run_code_from_file(file_path)
