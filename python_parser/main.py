from yacc_parser import parse

if __name__ == '__main__':
    file_name = input()
    f = open(file_name, "r")
    txt = f.read()
    term_list, nonterm_list, rules, error = parse(txt)
    print('Terminals:', ', '.join(term_list))
    print('Nonterminals:', ', '.join(nonterm_list))
    print('Rules:')
    for lhs, rhs in rules.items():
        print(' *', lhs, '->', ' | '.join(rhs))
    f.close()
