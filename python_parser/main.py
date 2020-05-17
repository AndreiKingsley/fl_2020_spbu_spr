from yacc_parser import parse


def list_concat(l):
    ans = ''
    for elem in l:
        ans += elem
    return ans


if __name__ == '__main__':
    file_name = input()
    f = open(file_name, "r")
    txt = f.read()
    term_list, nonterm_list, rules, error = parse(txt)
    print('Terminals:', ', '.join(sorted(list(term_list))))
    print('Nonterminals:', ', '.join(sorted(list(nonterm_list))))
    print('Rules:')
    for lhs, rhs in rules.items():
        print(' *', lhs, '->', ' | '.join(list(map(lambda x: list_concat(x), rhs))))
    f.close()
