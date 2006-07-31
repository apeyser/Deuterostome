|============== D machine for PPC: DSP1F generator ==================
|
| This acts as an extended macro preprocessor to the C compiler, which
| prepares nested, conditional, multi-line constructs in C.
|

1000 dict dup begin

/__   { * exch text } def
/st__ { st mkact __ } def
/dt__ { dt mkact __ } def
/op__ { op mkact __ } def
/NL   { * (\n) text } def


|----- macro argument stack routines

/A_stack 20 list def
/A_free 0 def

/pushA { 
  A_stack A_free put /A_free A_free 1 add def
} bind def

/popA {
  /A_free A_free 1 sub def A_stack A_free get
} bind def

|----------------------------- macros: make object value pointer
| (frame) | --    

/SCALAR { pushA (NUM_VAL\()__    popA __ (\))__ } def
/ARRAY  { pushA (VALUE_BASE\()__ popA __ (\))__ } def

|----------------------------- macro: get value via pointer (using cast),
| convert it from type into into double type, and save it in temporary
| variable; propagate an undefined value.

| /type { pointer_constructor } (tempname) | --    

/GETCV { dup pushA exch pushA pushA  GETCV_d exch get exec } def

/GETCV_d 6 dict dup begin

/B { NL (if \(\()__ popA __ ( = *\(\(B *\))__ popA exec (\)\) == BINF\) )__
        popA __ ( = HUGE_VAL;)__  
   } def
/W { NL (if \(\()__ popA __ ( = *\(\(W *\))__ popA exec (\)\) == WINF\) )__
        popA __ ( = HUGE_VAL;)__  
   } def
/L { NL (if \(\()__ popA __ ( = *\(\(L *\))__ popA exec (\)\) == LINF\) )__
        popA __ ( = HUGE_VAL;)__  
   } def
/S { NL popA __ ( = *\(\(S *\))__ popA exec (\);)__ popA pop
   } def
/D { NL popA __ ( = *\(\(D *\))__ popA exec (\);)__ popA pop
   } def
end def

|----------------------------- macro: build C code to convert a
| double value in a temporary variable into the destination type and
| to store the result at the destination:

|  /dest_type { dest_pointer_constructor } (tempname) | --    

/CVPUT { pushA pushA CVPUT_d exch get exec } def

/CVPUT_d 6 dict dup begin
/B { NL (*\(\(B *\))__ popA exec (\) = \(\(abs\()__
        popA dup pushA __ (\) > BMAX\) || isinf\()__
        popA dup pushA __ (\)\)? BINF : )__ popA __ (;)__
   } def
/W { NL (*\(\(W *\))__ popA exec (\) = \(\(abs\()__
        popA dup pushA __ (\) > WMAX\) || isinf\()__
        popA dup pushA __ (\)\)? WINF : )__ popA __ (;)__
   } def
/L { NL (*\(\(L *\))__ popA exec (\) = \(\(abs\()__
        popA dup pushA __ (\) > LMAX\) || isinf\()__
        popA dup pushA __ (\)\)? LINF : )__ popA __ (;)__
   } def
/S { NL (*\(\(S *\))__ popA exec (\) = )__ popA __ (;)__ 
   } def
/D { NL (*\(\(D *\))__ popA exec (\) = )__ popA __ (;)__ 
   } def
end def

|-------------------------------------------- construct C code of 'for'
| statement, which executes body as many times as array frame has
| elements (a long count variable, n, is assumed to exist);

| (framename) { body_constructor } | --

/FOR { pushA pushA
NL (for \(n = ARRAY_SIZE\()__ popA __ (\); n>0; n--\) {)__
popA exec
NL (})__
} def



|-------------------- interpolate
|
| static D clb[] = {
|     , , , ,
|     , , , ,
|   };
| 
| static void WDip(B *sf, B *df, W l2r)
| {
| W *s; D *d; L n,r;
| D *c, *cl; W k;
|
| s = (W *)VALUE_BASE(sf); d = (W *)VALUE_BASE(df);
| n = ARRAY_SIZE(sf)-3; r = 2 ** l2r;
| cl = clb + ((r - l2r -1)<<2); s++;
| while (n)
|    { *(d++) = *s; c = cl; k = r-1;
|      while (k) 
|        { *(d++) = c[0] * s[-1] + c[1] * s[0] + c[2] * s[1] + c[3] * s[2];
|          c += 4; k--;
|        }
|      s++; n--;
|    }
| *d = *s;
| }

/XYip {
NL
NL (static void )__ st__ dt__ (ip\(B *sf, B *df, W l2r\))__
NL ({)__
NL (D *c, *cl; W k; )__ st__ ( *s; )__ dt__ ( *d; L n,r;)__
NL (s = \()__ st__ ( *\)VALUE_BASE\(sf\); d = \()__ 
   dt__ ( *\)VALUE_BASE\(df\);)__
NL (n = ARRAY_SIZE\(sf\)-3; r = 1<<l2r;)__
NL (cl = clb + \(\(r - l2r -1\)<<2\); s++;)__
NL (while \(n\){ )__
NL (   *\(d++\) = *s; c = cl; k = r-1;)__
NL (   while \(k\))__ 
NL (    { *\(d++\) = c[0] * s[-1] + c[1] * s[0] + c[2] * s[1] + c[3] * s[2];)__
NL (      c += 4; k--; })__
NL (   s++; n--; })__
NL (*d = *s;)__
NL (})__
} def

|-------- coefficient list for Lagrange interpolations
| from Zurmuehl, p. 206
| powers_of_2 | --  (table output)

/numlitbuf 40 /b array def
/numlit {
   numlitbuf 0 * 4 -1 roll * number * (, ) text
   0 exch getinterval __} def

/SDipC { /po2 name
NL (static D clb[] = {)__
1 1 po2 { 2 exch pwr /xf name
     1.0 xf div /dx name
     1 1 xf 1 sub { /D ctype dx mul /x name
NL            x x 1 sub mul x 2 sub mul -6 div numlit
              x 1 add x 1 sub mul x 2 sub mul 2 div numlit
              x 1 add x mul x 2 sub mul -2 div numlit
NL            x 1 add x mul x 1 sub mul 6 div numlit
        } for
   } for
NL (};)__
NL
NL
} def

|------------------------------ extrema
|
| static void Wextr(B *sf, D *min, D *max)
| {
| W *s; D t; L n;
| s = (W *)VALUE_BASE(sf); n = ARRAY_SIZE(sf);
| while (--n)
|    { t = *(s++);
|      if (t<*min) { *min = t; } else { if (t>*max) *max = t; }
|    }
| }

/Xextr {
NL (static void )__ st__ (extr\(B *sf, D *min, D *max\))__
NL ({)__
NL st__ ( *s; D t; L n;)__
NL (s = \()__ st__ ( *\)VALUE_BASE\(sf\); n = ARRAY_SIZE\(sf\);)__
NL (while \(n--\))__
NL ( { t = *\(s++\);)__
NL (   if \(t<*min\) { *min = t; } else { if \(t>*max\) *max = t; })__
NL ( })__
NL (})__
NL
} def

|------------------------------ integrate (one-half rule)
|
| static void Wintegroh(B *sf)
| {
| W *s; D y; L n;
| s = (W *)VALUE_BASE(sf); n = ARRAY_SIZE(sf);
| y = *s; *(s++) = sum = 0.0;
| while (--n) { sum += y + *s; y = *s; *(s++) = sum; }
| }

/Xintegroh {
NL (static void )__ st__ (integroh\(B *sf\))__
NL ({)__
NL st__ ( *s; D y, sum; L n;)__
NL (s = \()__ st__ ( *\)VALUE_BASE\(sf\); n = ARRAY_SIZE\(sf\);)__
NL (y = *s; *\(s++\) = sum = 0.0;)__
NL (while \(--n\) { sum += y + *s; y = *s; *\(s++\) = sum; })__
NL (})__
NL
} def

|------------------------------ integrate (one-half rule with variable step)
|
| static void Wintegroh(B *sf, B *xf)
| {
| W *s, *x; D y; L n;
| s = (W *)VALUE_BASE(sf); n = ARRAY_SIZE(sf); x = (W *)VALUE_BASE(xf);
| y = *s; *(s++) = sum = 0.0;
| while (--n) { sum += (y + (*s)) * (*(x++)); y = *s; *(s++) = sum; }
| }

/Xintegrohv {
NL (static void )__ st__ (integrohv\(B *sf, B *xf\))__
NL ({)__
NL st__ ( *s, *x; D y, sum; L n;)__
NL (s = \()__ st__ ( *\)VALUE_BASE\(sf\); n = ARRAY_SIZE\(sf\);)__
   ( x = \()__ st__ ( *\)VALUE_BASE\(xf\) + 1;)__
NL (y = *s; *\(s++\) = sum = 0.0;)__
NL (while \(--n\) { sum += \(y + *s\) * \(*\(x++\)\); y = *s; *\(s++\) = sum; })__
NL (})__
NL
} def

|-------------------- construct DSP1F.C -----------------------------
| (path/) (file) | --

/all {
  {

NL
|----- generate DSP1_IP coefficient table
5 SDipC

|----- generate DSP1_IP function definitions
[ /B /W /L  /S /D ]
{  /st name 
   [ /B /W /L  /S /D ] {  /dt name XYip } forall
} forall

|----- generate definition of list of DSP1_IP functions

NL (typedef void \(*IPfct\)\(B *sf, B *df, W l2r\);)__
NL (static IPfct IPlist[] = {)__
NL
[ /B /W /L  /S /D ]
{  /st name 
   [ /B /W /L  /S /D ]
   {  /dt name 
      st__ dt__ (ip)__  (, )__
      NL
   } forall
} forall
NL(};)__
NL

|----- generate DSP1_EXTR function definitions
[ /B /W /L /S /D ] {  /st name Xextr } forall

|----- generate definition of list of DSP1_EXTR functions

NL (typedef void \(*EXTRfct\)\(B *sf, D *min, D *max\);)__
NL (static EXTRfct EXTRlist[] = {)__
NL
[ /B /W /L  /S /D ]
   { /st name 
      st__ (extr)__  (, )__
      NL
   } forall
NL(};)__
NL

|----- generate DSP1_INTEGRoh function definitions

[ /B /W /L /S /D ] {  /st name Xintegroh } forall

|----- generate definition of list of DSP1_INTEGRoh functions

NL (typedef void \(*INTEGRohfct\)\(B *sf\);)__
NL (static INTEGRohfct INTEGRohlist[] = {)__
NL
[ /B /W /L  /S /D ]
   { /st name 
      st__ (integroh)__  (, )__
      NL
   } forall
NL(};)__
NL

|----- generate DSP1_INTEGRoh function definitions

[ /B /W /L /S /D ] {  /st name Xintegrohv } forall

|----- generate definition of list of DSP1_INTEGRohV functions

NL (typedef void \(*INTEGRohVfct\)\(B *sf, B *xf\);)__
NL (static INTEGRohVfct INTEGRohVlist[] = {)__
NL
[ /B /W /L  /S /D ]
   { /st name 
      st__ (integrohv)__  (, )__
      NL
   } forall
NL(};)__
NL

} tofiles
} def

|------------------------ Testing constructors ------------------------
/testbuf 10000 /b array def

/ip { /dt name /st name
testbuf 0 SDip 0 exch getinterval /Rss name Rss toconsole
} def

/ipC { /p name
testbuf 0 p SDipC 0 exch getinterval /Rss name Rss toconsole
} def

/extr { /st name
testbuf 0 Xextr 0 exch getinterval /Rss name Rss toconsole
} def

/integroh { /st name
testbuf 0 Xintegroh 0 exch getinterval /Rss name Rss toconsole
} def

/integrohv { /st name
testbuf 0 Xintegrohv 0 exch getinterval /Rss name Rss toconsole
} def

end userdict /dsp1f put

