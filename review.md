# Ревью

## [Ольга Шиманская](https://github.com/jeinygroove/fl_2020_spbu_spr/)

Всё ок, язык нормальный, правда в новом синтаксисе не очень нравится, что естество функции никак не обособлено; документация хорошая, правда куда-то делся репортер ошибок, и их стало ложно искать((.

### Программки:

* Дикретный двоичный логарифм(с округлением вверх) -- программа: 
```
Def(power_of_two)(n)(Seq{Assign(a)(1); If (n == 0)(Seq {})(Seq{Assign (a)(2 * power_of_two(n - 1));});}) Return (a) Seq{Read (x); Assign(k)(0); While(x <= power_of_two(k))(Seq{Assign(k)(k + 1);}); Write(k);}
```

* Фиббоначи -- функция:
```
Def(fib)(n)(Seq{Assign(a)(1); If(n > 1)(Seq{Assign(a)(fib(n-1)+fib(n-2));})(Seq{});}) Return (a)
```

* Функция Аккермана - функция
```
Def(A)(m, n)(Seq{Assign(a)(n + 1); If(m == 0)(Seq{})(Seq{If(n == 0)(Seq{Assign(a)(A(m - 1, n));})(Seq{Assign(a)(A(m-1, A(m, n -1)));});});}) Return (a)
```

## [Василий Лупуляк](https://github.com/VasilyLupuleac/fl_2020_spbu_spr/)

Синаксис, документация -- всё класс.


### Программки:

* Перевод из двоичной в дестяичную -- функция:

```
fun fromBinToDeci(x) {if (x == 0) a = 0 else a = 2 * (fromBinToDeci(x / 10) + x % 10); return a}
```
	
* Большая программа(надеюсь сойдёт за две). Две функции -- факториал и возведение в степень; сама программа считаем экспоненту в степени x с точностью до k-ый степени x-а.
 
```
fun fact(y) {a = 1; if (y /= 1) a = y * fact(y - 1); return a} fun pow(y, b) {a = 1; if (b /= 0) a = y * pow(y, b - 1); return a} {read(x); read(k); a = 1; while (k /= 0) {a = a + pow(x, k) / fact(k);}; write(a);}
```
