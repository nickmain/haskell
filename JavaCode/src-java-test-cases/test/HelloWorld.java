package test;

import java.io.IOException;
import java.io.Serializable;
import java.lang.annotation.RetentionPolicy;

@Deprecated
@Anno( foo="test", boo=true, value=23, 
       bit=12, cha='a', sho=9, flo=11.1f, 
       dub=22.2, cla=String[][].class, policy=@Ann2(policy=RetentionPolicy.CLASS),
       arr={ @Ann2(policy=RetentionPolicy.RUNTIME), @Ann2(policy=RetentionPolicy.CLASS) })
public class HelloWorld implements Cloneable, Serializable {

	private static final long serialVersionUID = 1247745283845014052L;
	
    @Anno( foo="test", boo=true, value=23, 
            bit=12, cha='a', sho=9, flo=11.1f, 
            dub=22.2, cla=String[][].class, policy=@Ann2(policy=RetentionPolicy.CLASS),
            arr={ @Ann2(policy=RetentionPolicy.RUNTIME), @Ann2(policy=RetentionPolicy.CLASS) })
	@Ann2(policy=RetentionPolicy.RUNTIME) private int foo = 1;
	
	public HelloWorld() {}
	
    @Override
	public String toString() {
        System.out.println( "before try.." );
        
        try {
            return  Anno.class.getName();
        } catch( Exception ex ) {
            System.out.println( "whoa !!" );
        }

        return "Hello World";
	}
    
    public boolean foo( String a, int b, char... c )
        throws IOException, IllegalArgumentException {
        
        if( a == null ) {
            System.out.println( "a" );
        } else {
            System.out.println( "b" );
        }
     
        switch( b ) {
            case 10: return true;
            case 20: return false;
            case 30: return true;
            default : break;
        }
        
        return true;
    }
    
    public class HWInner {
    }
}
