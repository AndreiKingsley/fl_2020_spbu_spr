from ply import lex, yacc
from collections import defaultdict

tokens = (
    'TERM',
    'NONTERM',
    'EPS',
    'ARROW',
    'VBAR',
    'N'
)

t_TERM = r'\"[a-z]+\"'
t_NONTERM = r'<[A-Z]+>'
t_EPS = r'_'
t_ARROW = r'->'
t_VBAR = r'\|'
t_N = r'\n'

t_ignore = ' '

error = False


def t_error(t):
    global error
    error = True
    print('Illegal character {}'.format(t.value[0]))
    t.lexer.skip(1)


lexer = lex.lex()

term_list = set()
nonterm_list = set()
rules = defaultdict(list)


def p_one_rule(p):
    '''rule : NONTERM ARROW rhs N'''
    nonterm_list.add(p[1])
    rules[p[1]] += (p[3])


def p_many_rules(p):
    '''rule : NONTERM ARROW rhs N rule'''
    nonterm_list.add(p[1])
    rules[p[1]] += (p[3])


def p_single_term(p):
    '''rhs : TERM'''
    term_list.add(p[1])
    p[0] = []
    p[0].append(p[1])


def p_single_nonterm(p):
    '''rhs : NONTERM'''
    nonterm_list.add(p[1])
    p[0] = []
    p[0].append(p[1])


def p_single_eps(p):
    '''rhs : EPS'''
    p[0] = []
    p[0].append(p[1])


def p_rhs_eps(p):
    '''rhs : rhs VBAR EPS'''
    p[0] = p[1].copy()
    p[0].append(p[3])


def p_rhs_term(p):
    '''rhs : rhs VBAR TERM'''
    term_list.add(p[3])
    p[0] = p[1].copy()
    p[0].append(p[3])


def p_rhs_nonterm(p):
    '''rhs : rhs VBAR NONTERM'''
    nonterm_list.add(p[3])
    p[0] = p[1].copy()
    p[0].append(p[3])


def p_error(p):
    global error
    error = True
    print('Syntax error at {}'.format(p))


def parse(s):
    global rules, term_list, nonterm_list, error
    term_list.clear()
    nonterm_list.clear()
    rules.clear()
    error = False
    yacc.parse(s)
    return term_list, nonterm_list, normalize_rules(rules), error


parser = yacc.yacc()


def normalize_rules(rules):
    new_rules = defaultdict(list)
    for k, val in rules.items():
        val_set = set(val)
        new_rules[k] = sorted(list(val_set))
    return new_rules
