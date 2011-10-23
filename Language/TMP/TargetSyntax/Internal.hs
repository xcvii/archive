{-
   This file is part of HsTMP.

   HsTMP -- C++ template metaprogram preprocessor
   Copyright (C) 2011  Endre Tamas SAJO
 
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
 
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
 
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Language.TMP.TargetSyntax.Internal
  (
      Name (..)
    , Builtin (..)
    , Param (..)
    , Arg (..)
    , Basic (..)
    , Expr (..)
    , Decl (..)
  )
  where

import Data.List (intercalate)
import Language.TMP.PP

data Name = Name String
  deriving (Show)

instance PP Name where
  pprint (Name name) = name


data Builtin =
    IntT
  | BoolT
  | Other String
  deriving (Show)

instance PP Builtin where
  pprint IntT = "int"
  pprint BoolT = "bool"
  pprint (Other s) = s


data Param =
    TypeP Name
  | IntP Name
  | BoolP Name
  deriving (Show)

data Arg =
    IntA Int
  | BoolA Bool
  deriving (Show)

instance PP Param where
  pprint (TypeP name) = pprint name
  pprint (IntP name) = pprint name
  pprint (BoolP name) = pprint name

instance PP Arg where
  pprint (IntA n) = show n
  pprint (BoolA b) = case b of True -> "true"; False -> "false"


data Basic =
    Builtin Builtin
  | Param Param
  | Arg Arg
  | UDT Name
  | Ptr Basic
  | Const Basic
  | Ref Basic
  deriving (Show)

instance PP Basic where
  pprint (Builtin builtin) = pprint builtin
  pprint (Param param) = pprint param
  pprint (Arg arg) = pprint arg
  pprint (UDT name) = pprint name
  pprint (Ptr basic) = unwords [pprint basic, "*"]
  pprint (Const basic) = unwords [pprint basic, "const"]
  pprint (Ref basic) = unwords [pprint basic, "&"]


data Expr =
    BasicE Basic
  | StructE [Expr] [Decl]
  | EnumE [(Name, Maybe Int)]
  | InstanceE Name [Expr]
  | NestedE Expr Expr
  deriving (Show)

instance PP Expr where
  pprint (BasicE basic) = pprint basic

  pprint (StructE base nested) =
    unwords $ concat [["struct"], pr base, ["{"], map pprint nested, ["}"]]
    where pr [] = []
          pr es = [":", intercalate ", " $ map pprint es]

  pprint (EnumE elems) =
    unwords ["enum", "{", intercalate ", " $ map pr elems, "}"]

    where pr (name, Nothing) = pprint name
          pr (name, Just n) = unwords [pprint name, "=", show n]

  pprint (InstanceE name params) =
    unwords [pprint name, "<", intercalate ", " $ map pprint params, ">"]

  pprint (NestedE outer inner) =
    unwords $ concat [typenameQ,
                      [pprint outer],
                      ["::"], templateQ, [pprint inner]] 

    where (typenameQ, templateQ) = case outer of
                (BasicE (Param _)) -> case inner of
                                      (InstanceE _ _) -> ([], ["template"])
                                      _ -> (["typename"], [])
                _ -> ([], [])

data Decl =
    TypedefD Name Expr
  | StructFwdD Name
  | StructD Name [Expr] [Decl]
  | EnumD Name [(Name, Maybe Int)]
  | TemplateFwdD Name [Param]
  | TemplateD Name [Param] [Decl]
  | SpecD Name [Param] [Expr] [Decl]
  deriving (Show)

instance PP Decl where
  pprint (TypedefD name exp) =
    unwords ["typedef", pprint exp, pprint name] ++ ";"

  pprint (StructFwdD name) = unwords ["struct", pprint name, ";"]

  pprint (StructD name base nested) =
    unwords $ concat [["struct", pprint name], pr base, ["{"],
                      map pprint nested, ["};"]]
    where pr [] = []
          pr es = [":", intercalate ", " $ map pprint es]

  pprint (EnumD name elems) =
    unwords ["enum", pprint name, "{", intercalate ", " $ map pr elems, "};"]

    where pr (name, Nothing) = pprint name
          pr (name, Just n) = unwords [pprint name, "=", show n]

  pprint (TemplateFwdD name params) =
    unwords $ concat [["template"], ["<"], map pr params, [">"],
                      ["struct", pprint name], [";"]]

    where pr (TypeP name) = unwords ["typename", pprint name]
          pr (IntP name) = unwords ["int", pprint name]
          pr (BoolP name) = unwords ["bool", pprint name]

  pprint (TemplateD name params nested) =
    unwords $ concat [["template"], ["<"], map pr params, [">"],
                      ["struct", pprint name],
                      ["{"], map pprint nested, ["};"]]

    where pr (TypeP name) = unwords ["typename", pprint name]
          pr (IntP name) = unwords ["int", pprint name]
          pr (BoolP name) = unwords ["bool", pprint name]

  pprint (SpecD name params exprs nested) =
    unwords $ concat [["template", "<"], map pr params, [">"],
                      ["struct", pprint name, "<"], map pprint exprs, [">"],
                      ["{"], map pprint nested, ["};"]]

    where pr (TypeP name) = unwords ["typename", pprint name]
          pr (IntP name) = unwords ["int", pprint name]
          pr (BoolP name) = unwords ["bool", pprint name]


