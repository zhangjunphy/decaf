-- Copyright (C) 2018-2024 Jun Zhang <zhangjunphy[at]gmail[dot]com>
--
-- This file is a part of decafc.
--
-- decafc is free software: you can redistribute it and/or modify it under the
-- terms of the MIT (X11) License as described in the LICENSE file.
--
-- decafc is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the X11 license for more details.

-- Lexer tokens
module Lexer.Token (Token(..)) where

import Data.Text (Text)

-- | Lexer tokens.
data Token = Keyword !Text
           | Identifier !Text
           | CharLiteral !Text
           | IntLiteral !Text
           | BooleanLiteral !Text
           | StringLiteral !Text
           | AssignOp
           | CompoundAssignOp !Text
           | IncrementOp !Text
           | ArithmeticOp !Text
           | RelationOp !Text
           | EquationOp !Text
           | ConditionOp !Text
           | LCurly
           | RCurly
           | LParen
           | RParen
           | LBrack
           | RBrack
           | Choice
           | Colon
           | Semicolon
           | Comma
           | Negate
           | EOF
           deriving (Eq)

instance Show Token where
  show (Keyword k) = show k
  show (Identifier s) = "IDENTIFIER " ++ show s
  show (CharLiteral s) = "CHARLITERAL " ++ show s
  show (IntLiteral s) = "INTLITERAL " ++ show s
  show (BooleanLiteral s) = "BOOLEANLITERAL " ++ show s
  show (StringLiteral s) = "STRINGLITERAL " ++ show s
  show AssignOp = "="
  show (IncrementOp s) = show s
  show (CompoundAssignOp s) = show s
  show (ArithmeticOp s) = show s
  show (RelationOp s) = show s
  show (EquationOp s) = show s
  show (ConditionOp s) = show s
  show LCurly = "{"
  show RCurly = "}"
  show LParen = "("
  show RParen = ")"
  show LBrack = "["
  show RBrack = "]"
  show Choice = "?"
  show Colon = ":"
  show Semicolon = ";"
  show Comma = ","
  show Negate = "!"
  show EOF = "EOF"
