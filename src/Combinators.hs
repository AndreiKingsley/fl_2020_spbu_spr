﻿module Combinators where

import           Control.Applicative


data Result error input result
  = Success input result
  | Failure error
  deriving (Show, Eq)

newtype Parser error input result
  = Parser { runParser :: input -> Result error input result }

instance Functor (Parser error input) where
  fmap f p = Parser $ \input ->
  	case runParser p input of
        	Success i r -> Success i (f r)
         	Failure e   -> Failure e

instance Applicative (Parser error input) where
  pure x = Parser $ \input -> Success input x

  p1 <*> p2 = Parser $ \input -> 
	case runParser p1 input of
		Failure e    -> Failure e
		Success i rf -> case runParser p2 i of
			Success i' r' -> Success i' (rf r')
			Failure e     -> Failure e
			

instance Monad (Parser error input) where
  return x = Parser $ \input -> Success input x

  p >>= f = Parser $ \input ->
  	case runParser p input of
    		Success i r -> runParser (f r) i
    		Failure e   -> Failure e


instance Monoid error => Alternative (Parser error input) where
  empty = Parser $ \i -> Failure mempty

  p <|> q = Parser $ \input -> 
	case runParser p input of
		Failure _ -> runParser q input                 
		x         -> x		

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sep elem = (:) <$> elem <*> (many (sep *> elem))

-- Проверяет, что первый элемент входной последовательности -- данный символ
symbol :: Char -> Parser String String Char
symbol c = satisfy (== c)

-- Успешно завершается, если последовательность содержит как минимум один элемент
elem' :: (Show a) => Parser String [a] a
elem' = satisfy (const True)

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату
satisfy :: Show a => (a -> Bool) -> Parser String [a] a
satisfy p = Parser $ \input ->
  case input of
    (x:xs) | p x -> Success xs x
    _            -> Failure $ "Predicate failed"

-- Успешно парсит пустую строку
epsilon :: Parser e i ()
epsilon = return ()

fail' :: e -> Parser e i a
fail' = Parser . const . Failure

