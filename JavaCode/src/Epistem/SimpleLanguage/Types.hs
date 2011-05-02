-- | Commons types
module Epistem.SimpleLanguage.Types where {

    import Data.Int;

    -- | The language value types
    data Type = TypeObject String
              | TypeArray  Type
              | TypeVoid
              | TypeInt
              | TypeLong
              | TypeByte
              | TypeShort
              | TypeChar
              | TypeBool
              | TypeFloat
              | TypeDouble
              ;
              
    -- | Constant values
    data Constant = ConstInt    Int32
                  | ConstLong   Int64
                  | ConstFloat  Float
                  | ConstDouble Double
                  | ConstString String
                  | ConstChar   Char
                  | ConstBool   Bool
                  | ConstType   Type
                  | ConstNull
                  ;
}