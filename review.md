# Ревью

## [Аля Новикова](https://github.com/AlyaNovikova/fl_2020_spbu_spr)

Нормальная грамматика, но для самоописания не подходит, поэтому в кое каких местах "напсевдокодил".

```
@S := @Seq
@Seq := @Rule + "(newline)"
@Seq := @Rule + "(newline)" + @Seq
@Rule := @Nonterm + ":=" + @Rhs
@Rhs := 
@Rhs := @Sym
@Rhs := @Sym + "+" + @Rhs
@Sym := @Term
@Sym := @Nonterm
@Nonterm := "(sobachka)" + @StringLetter
@Term := "\"" + @StringSymbol + "\""
@StringLetter := @Letter
@StringLetter := @Letter + @StringLetter
@StringSymbol := @Symbol
@StringSymbol := @Symbol + @StringSymbol
@Symbol := @Letter
@Symbol := @Digit
@Symbol := @Other
@Letter := "(a-z)"
@Letter := "(A-Z)"
@Digit := "(0-9)"
@Other := ":=+-*/()\""

```

## [Иван Павлов]

Отлично круто, я запарился, написал полностью описание грамматики, но она не LL-ная и поэтому не парситься((

```
<S>:=<RulesSeq>
<RulesSeq>:=<RulesSeq>'\n'<Rule>
<RulesSeq>:=<Rule>
<Rule>:=<Lhs>':''='<RhsSeq>
<Lhs>:=<Nonterminal>
<RhsSeq>:=<RhsSeqNotEps>
<RhsSeq>:=<Eps>
<Eps>:=''<'eps'>''
<RhsSeqNotEps>:=<RhsSeqNotEps><Rhs>
<RhsSeqNotEps>:=<Rhs>
<Rhs>:=<RhsElem><Rhs>
<Rhs>:=<RhsElem>
<RhsElem>:=<Term>
<RhsElem>:=<Nonterm>
<RhsElem>:=<ExtraTerm>
<Eps>:=''<'eps'>''
<Nonterm>:='<'<SymbSeq>'>'
<Term>:=<Symb>
<ExtraTerm>:=<ExtraSymb>
<SymbSeq>:=<SymbSeq><Symb>
<SymbSeq>:=<Symb>
<Symb>:=a
<Symb>:=b
<Symb>:=c
<Symb>:=d
<Symb>:=e
<Symb>:=f
<Symb>:=g
<Symb>:=h
<Symb>:=i
<Symb>:=j
<Symb>:=k
<Symb>:=l
<Symb>:=m
<Symb>:=n
<Symb>:=o
<Symb>:=p
<Symb>:=q
<Symb>:=r
<Symb>:=s
<Symb>:=t
<Symb>:=u
<Symb>:=v
<Symb>:=w
<Symb>:=x
<Symb>:=y
<Symb>:=z
<Symb>:=A
<Symb>:=B
<Symb>:=C
<Symb>:=D
<Symb>:=E
<Symb>:=F
<Symb>:=G
<Symb>:=H
<Symb>:=I
<Symb>:=J
<Symb>:=K
<Symb>:=L
<Symb>:=M
<Symb>:=N
<Symb>:=O
<Symb>:=P
<Symb>:=Q
<Symb>:=R
<Symb>:=S
<Symb>:=T
<Symb>:=U
<Symb>:=V
<Symb>:=W
<Symb>:=X
<Symb>:=Y
<Symb>:=Z
<Symb>:=0
<Symb>:=1
<Symb>:=2
<Symb>:=3
<Symb>:=4
<Symb>:=5
<Symb>:=6
<Symb>:=7
<Symb>:=8
<Symb>:=9
<ExtraSymb>:='' ''
<ExtraSymb>:='<'
<ExtraSymb>:='>'
<ExtraSymb>:=''\n''
<ExtraSymb>:=''\t''
<ExtraSymb>:='':''
<ExtraSymb>:=''=''
```