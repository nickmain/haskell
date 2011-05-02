package test;

public @interface Anno {
    int value() default 1;
    String  foo() default "";
    boolean boo() default false;
    short   sho() default 0;
    long    loo() default 0L;
    byte    bit() default 0;
    float   flo() default 0f;
    double  dub() default 0.0;
    char    cha() default ' ';
    Class   cla();
    Ann2    policy();
    Ann2[]  arr();
}
