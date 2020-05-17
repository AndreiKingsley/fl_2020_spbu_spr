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
    '''rule : NONTERM ARROW seq N'''
    nonterm_list.add(p[1])
    rules[p[1]] += (p[3])


def p_many_rules(p):
    '''rule : NONTERM ARROW seq N rule'''
    nonterm_list.add(p[1])
    rules[p[1]] += (p[3])


def p_single_seq(p):
    '''seq : rhs'''
    p[0] = [p[1]]


def p_multiple_seq(p):
    '''seq : seq VBAR rhs'''
    p[0] = p[1].copy()
    p[0].append(p[3])


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
    '''rhs : rhs EPS'''
    p[0] = p[1].copy()
    p[0].append(p[2])


def p_rhs_term(p):
    '''rhs : rhs TERM'''
    term_list.add(p[2])
    p[0] = p[1].copy()
    p[0].append(p[2])


def p_rhs_nonterm(p):
    '''rhs : rhs NONTERM'''
    nonterm_list.add(p[2])
    p[0] = p[1].copy()
    p[0].append(p[2])


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
    return term_list, nonterm_list, unique_dict(rules), error


parser = yacc.yacc()


def unique_list(l):
    ans = []
    for elem in l:
        if not (elem in ans):
            ans.append(elem)
    return ans


def unique_dict(dict):
    for k, val in dict.items():
        dict[k] = sorted(unique_list(val))
    return dict
