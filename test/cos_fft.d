/stdio 10 dict dup begin
/buf 1024 /b array def
/printe {
  /n name
  buf 0 {
    n 0 le ~exit if
    * n 3 add -1 roll * number (   ) fax
    /n n 1 sub def
  } loop
  (\n) fax 0 exch getinterval toconsole
} bind def
end def

/fft 10 dict dup begin
/Wfft1 1024 /d array def
/Wfft2 Wfft1 length /d array def
/Wfft3 Wfft1 length /d array def
/Pi -1d acos def

/subset 1024 def

/test {
  Wfft1 0 Wfft1 length 0.0 2.0 Pi mul 1024 div ramp pop
  Wfft2 copy cos 
  Wfft3 copy 1 realFFT -1 realFFT pop

  0 1 subset 1 sub {/k name 
    Wfft1 k get 
    Wfft2 k get 
    Wfft3 k get 
    3 stdio begin printe end 
  } for
} bind def
end def
