import pytest
from yacc_parser import parse


def test_1():
    txt = '<S> -> "a" | <S>\n'
    term_list, nonterm_list, rules, error = parse(txt)
    assert not error
    assert nonterm_list == {'<S>'}
    assert term_list == {'"a"'}
    assert rules == {'<S>': ['"a"', '<S>']}


def test_2():
    txt = '<S> -> "ab" | <R> | _ \n <R> -> <S>\n'
    term_list, nonterm_list, rules, error = parse(txt)
    assert not error
    assert nonterm_list == {'<S>', '<R>'}
    assert term_list == {'"ab"'}
    assert rules == {'<S>': ['"ab"', '<R>', '_'], '<R>': ['<S>']}


def test_error1():
    txt = '<S> -> "T"\n'
    _, _, _, error = parse(txt)
    assert error


def test_error2():
    txt = '<s> -> "t"\n'
    _, _, _, error = parse(txt)
    assert error


def test_multidef():
    txt = '<S> -> "a" | "b" \n <S> -> "a" | <T>\n'
    term_list, nonterm_list, rules, error = parse(txt)
    assert not error
    assert nonterm_list == {'<S>', '<T>'}
    assert term_list == {'"a"', '"b"'}
    assert rules == {'<S>': ['"a"', '"b"', '<T>']}


if __name__ == '__main__':
    pytest.main()
