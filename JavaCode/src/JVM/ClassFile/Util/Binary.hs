-- | Binary IO Types and utils
module JVM.ClassFile.Util.Binary where {

    import qualified Data.ByteString as BS;

    type Bytes = BS.ByteString;

    class Binary a where {        
        writeBin :: a -> Bytes;
        readBin  :: Bytes -> a; 
    };

}
