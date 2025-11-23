module Test.Printing (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Charon.Printing
import Generated_Types
import Generated_Expressions
import Generated_Values

tests :: TestTree
tests = testGroup "Printing Tests"
  [ testGroup "Basic Types"
      [ testCase "Print IntTy" $ do
          printWithCtx emptyCtx I32 @?= "i32"
          printWithCtx emptyCtx I64 @?= "i64"
      
      , testCase "Print UIntTy" $ do
          printWithCtx emptyCtx U32 @?= "u32"
          printWithCtx emptyCtx U64 @?= "u64"
      
      , testCase "Print FloatType" $ do
          printWithCtx emptyCtx F32 @?= "f32"
          printWithCtx emptyCtx F64 @?= "f64"
      
      , testCase "Print RefKind" $ do
          printWithCtx emptyCtx RShared @?= "shared"
          printWithCtx emptyCtx RMut @?= "mut"
      
      , testCase "Print BorrowKind" $ do
          printWithCtx emptyCtx BShared @?= "Shared"
          printWithCtx emptyCtx BMut @?= "Mut"
      ]
  
  , testGroup "Operators"
      [ testCase "Print Binop" $ do
          printWithCtx emptyCtx BitXor @?= "^"
          printWithCtx emptyCtx BitAnd @?= "&"
          printWithCtx emptyCtx Eq @?= "=="
          printWithCtx emptyCtx Lt @?= "<"
      
      , testCase "Print Binop with OverflowMode" $ do
          printWithCtx emptyCtx (Add OPanic) @?= "panic.+"
          printWithCtx emptyCtx (Sub OWrap) @?= "wrap.-"
      
      , testCase "Print Unop" $ do
          printWithCtx emptyCtx Not @?= "~"
          printWithCtx emptyCtx (Neg OPanic) @?= "panic.-"
      
      , testCase "Print OverflowMode" $ do
          printWithCtx emptyCtx OPanic @?= "panic"
          printWithCtx emptyCtx OWrap @?= "wrap"
          printWithCtx emptyCtx Oub @?= "ub"
      ]
  
  , testGroup "Abort and Names"
      [ testCase "Print AbortKind without name" $ do
          printWithCtx emptyCtx (Panic Nothing) @?= "panic"
      
      , testCase "Print AbortKind" $ do
          printWithCtx emptyCtx UndefinedBehavior @?= "undefined_behavior"
          printWithCtx emptyCtx UnwindTerminate @?= "unwind_terminate"
      
      , testCase "Print Name" $ do
          let name = Name [PeIdent "std" (Disambiguator 0), PeIdent "vec" (Disambiguator 0)]
          printWithCtx emptyCtx name @?= "std::vec"
      
      , testCase "Print Name with disambiguator" $ do
          let name = Name [PeIdent "foo" (Disambiguator 1)]
          printWithCtx emptyCtx name @?= "foo#1"
      ]
  
  , testGroup "Built-in Functions"
      [ testCase "Print BuiltinFunId" $ do
          printWithCtx emptyCtx BoxNew @?= "BoxNew"
          printWithCtx emptyCtx ArrayToSliceShared @?= "ArrayToSliceShared"
      
      , testCase "Print BuiltinIndexOp" $ do
          let indexOp = Index (BuiltinIndexOp True RShared False)
          printWithCtx emptyCtx indexOp @?= "ArrayIndexShared"
      
      , testCase "Print PtrFromParts" $ do
          printWithCtx emptyCtx (PtrFromParts RShared) @?= "PtrFromPartsShared"
          printWithCtx emptyCtx (PtrFromParts RMut) @?= "PtrFromPartsMut"
      ]
  
  , testGroup "IDs"
      [ testCase "Print LocalId" $ do
          printWithCtx emptyCtx (LocalId 0) @?= "@0"
          printWithCtx emptyCtx (LocalId 5) @?= "@5"
      
      , testCase "Print FieldId" $ do
          printWithCtx emptyCtx (FieldId 0) @?= "0"
          printWithCtx emptyCtx (FieldId 2) @?= "2"
      
      , testCase "Print VariantId" $ do
          printWithCtx emptyCtx (VariantId 1) @?= "1"
      ]
  ]
