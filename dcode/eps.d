| -*- mode: d; -*-

/EPS 100 {
  |------------ RESOLUTION -------------------
  |
  | The resolution argument to ghostscript.
  | Either -rX or -rXxY.
  |
  /RESOLUTION (-r2688) def

  |-------------- latex ----------------------
  |
  | The latex document is divided into 3 pieces:
  |  predoc is the everything up to the optional
  |    document class parameters
  |  pre is everything from the end of the optional
  |    documentclass parameters to the optional preamble.
  | main is the embedded
  |    macro. Basically, everything -- the gist is
  |    defined in \eps.
  | post is from the end of the main parameter to the
  |   end of the document.
  |
  | The document is a report, with the fonts defaulted to
  |   san-serif. The two parameters are 'XXpt' to set the
  |   the default point size for the report, and the actual
  |   latex to be embedded.
  |

  /predoc (\\documentclass[) def

  /pre (pt]{report}
\\input{sfonts.def}
\\def\\rmdefault{cmss}
\\def\\sfdefault{cmss}
\\def\\ttdefault{cmtt}
\\SetSymbolFont{operators}{normal}
                 {OT1}{cmss}{m}{n}
\\SetSymbolFont{letters}{normal}
                 {OML}{cmm}{m}{it}
\\SetSymbolFont{symbols}{normal}
                 {OMS}{cmsy}{m}{n}
\\SetSymbolFont{largesymbols}{normal}
                 {OMX}{cmex}{m}{n}

\\usepackage[margin=0pt,papersize={216in,216in}]{geometry}

\\newwrite\\epscomments
\\immediate\\openout\\epscomments\\jobname.comment

\\newbox\\epsbox
\\newlength\\epswidth
\\newlength\\epsheight
\\newlength\\epsdepth

\\pdfcompresslevel=0
\\begingroup
\\catcode`\\%=12
\\catcode`\\|=14
\\gdef\\comment#1#2{|
  \\immediate\\write\\epscomments{%%Latex#1:\\space#2}|
  \\typeout{#1:\\space#2}|
}
\\endgroup

\\newcommand\eps[1]{
  \\begin{document}%
      \\savebox\\epsbox{#1}%
      \\settowidth\\epswidth{\\usebox\\epsbox}%
      \\settoheight\\epsheight{\\usebox\\epsbox}%
      \\settodepth\\epsdepth{\\usebox\\epsbox}%
      \\comment{Width}{\\the\\epswidth}%
      \\comment{Height}{\\the\\epsheight}%
      \\comment{Depth}{\\the\\epsdepth}%
      \\usebox{\\epsbox}%
  \\end{document}
}%
) def
  /main(%
\\eps{) def

  /post (}\\immediate\\closeout\\epscomment) def

  |----------------------- eps_ -------------------
  |
  | -- <</input, ptsize, wr, ewr defined>> | --
  |
  | Here are the sinews. eps_ does all the scripting work,
  |  constructing a latex document out of ptsize (10,11 or 12)
  |  and input, an arbitary latex string.
  | It creates a temporary directory, puts the document in there,
  |   calls pdflatex on it, uses ghostscript to make it a cropped eps,
  |   uses sed to append the latex bounding box details (and strip pt
  |   from those numbers, since I can't seem to make latex do that),
  |   and finally appends an %%EOF to the entire thing.
  | The output sits in a pipe, as well as an error messages from
  |  the subprocesses.
  |
  /eps_ {
    null (eps) tmpdir /tsdir name /tdir name
    ewr (Working in temporary directory: `) writefd
    tdir writefd tsdir writefd
    ('\n) writefd pop

    /pwd getwdir def
    tdir tsdir setwdirp {{{
      (.) (eps.tex) wropen {
        predoc {(XX) 0 * ptsize * number pop} pre preamble main input post
      } {exec writefd} forall close

      [(pdflatex) (--halt-on-error) (--interaction=nonstopmode) (eps.tex)
        NULLR ewr dup sh_ wait not {true /estreamwith exitto} if |]

      [(gs) (-q) RESOLUTION (-dLanguageLevel=3)
        (-dNOPAUSE) (-dBATCH) (-dSAFER)
        (-sDEVICE=epswrite) (-sOutputFile=-)
        (eps.pdf)
        NULLR wr ewr sh_ wait not {true /estreamwith exitto} if |]

      [(sed) (-e) (s/pt$//) (eps.comment)
        NULLR wr ewr sh_ wait not {true /estreamwith exitto} if |]

      wr (%%EOF) writefd close
      false
    } /estreamwith exitlabel} stopped} aborted
    pwd setwdir
    ~abort if {true stop} if {true /estreamwith exitto} if

    tdir tsdir removedir
  } bind def

  |------------------------ eps ---------------------------
  |
  | (latex) ptsize | (eps)
  |
  | same as xeps, except that preamble is empty
  |
  /eps {() 3 1 roll xeps} bind def

  |------------------------ xeps ---------------------------
  |
  | (preamble) (latex) ptsize | (eps)
  |
  | (preamble) is any string that can go in the preamble
  |   after \documentclass[xpt]{report} and the standard preamble,
  |   and before \begin{document}.
  | (latex) is any movable latex string that can go between
  |   \document{begin}...\document{end} (it actually goes in
  |   \eps{...}, which then expands to the document)
  | ptsize can be 10, 11, or 12 to set the base font size
  | (eps) is the body for an eps file.
  | On error, the eps prints "hamuti".
  |
  | The resolution is defined by RESOLUTION defined in this
  |   module -- defaults to 1200 dpi.
  |
  | eps calls eps_, after defining ptsize and input, setting up
  |  input and output streams, and wrapping it in an error catcher
  |  that will dump the error stream to console if a stop or error
  |  happen internally.
  |
  /xeps { /ptsize name /input name /preamble name
    {
      /_eps {
        pipefd /wr name /rd name
        openlist {/ewr name eps_} ~estreamwith stopped {
          wr closeifopen
          rd closeifopen
          stop
        } if
      } layerdef

      {pop rd suckfd} {
        cleartomark
        wr closeifopen
        rd suckfd toconsole
        hamuti
      } ifelse
    } PROCESSES underdict
  } bind def

  | Hamuti!
  /hamuti <B
  37   33   80   83   45   65  100  111   98  101   45   51   46   48
  32   69   80   83   70   45   51   46   48   10   37   37   66  111
  117  110  100  105  110  103   66  111  120   58   32   40   97  116
  101  110  100   41   10   37   37   72  105   82  101  115   66  111
  117  110  100  105  110  103   66  111  120   58   32   40   97  116
  101  110  100   41   10   37   37   67  114  101   97  116  111  114
  58   32   71   80   76   32   71  104  111  115  116  115   99  114
  105  112  116   32   56   54   52   32   40  101  112  115  119  114
  105  116  101   41   10   37   37   67  114  101   97  116  105  111
  110   68   97  116  101   58   32   50   48   48   57   47   48   53
  47   50   50   32   50   49   58   48   55   58   53   50   10   37
  37   68  111   99  117  109  101  110  116   68   97  116   97   58
  32   67  108  101   97  110   55   66  105  116   10   37   37   76
  97  110  103  117   97  103  101   76  101  118  101  108   58   32
  51   10   37   37   69  110  100   67  111  109  109  101  110  116
  115   10   37   37   66  101  103  105  110   80  114  111  108  111
  103   10   37   32   84  104  105  115   32   99  111  112  121  114
  105  103  104  116   32   97  112  112  108  105  101  115   32  116
  111   32  101  118  101  114  121  116  104  105  110  103   32   98
  101  116  119  101  101  110   32  104  101  114  101   32   97  110
  100   32  116  104  101   32   37   37   69  110  100   80  114  111
  108  111  103   58   10   37   32   67  111  112  121  114  105  103
  104  116   32   40   67   41   32   50   48   48   57   32   65  114
  116  105  102  101  120   32   83  111  102  116  119   97  114  101
  44   32   73  110   99   46   32   32   65  108  108   32  114  105
  103  104  116  115   32  114  101  115  101  114  118  101  100   46
  10   37   37   66  101  103  105  110   82  101  115  111  117  114
  99  101   58   32  112  114  111   99  115  101  116   32   71   83
  95  101  112  115  119  114  105  116  101   95   51   95   48   95
  49   48   48   49   32   49   46   48   48   49   32   48   10   47
  71   83   95  101  112  115  119  114  105  116  101   95   51   95
  48   95   49   48   48   49   32   56   48   32  100  105   99  116
  32  100  117  112   32   98  101  103  105  110   10   47   80   97
  103  101   83  105  122  101   32   50   32   97  114  114   97  121
  32  100  101  102   47  115  101  116  112   97  103  101  115  105
  122  101  123   32   80   97  103  101   83  105  122  101   32   97
  108  111   97  100   32  112  111  112   32   51   32  105  110  100
  101  120   32  101  113   32  101  120   99  104   10   52   32  105
  110  100  101  120   32  101  113   32   97  110  100  123   32  112
  111  112   32  112  111  112   32  112  111  112  125  123   32   80
  97  103  101   83  105  122  101   32  100  117  112   32   32   49
  10   53   32   45   49   32  114  111  108  108   32  112  117  116
  32   48   32   52   32   45   49   32  114  111  108  108   32  112
  117  116   32  100  117  112   32  110  117  108  108   32  101  113
  32  123  102   97  108  115  101  125   32  123  100  117  112   32
  119  104  101  114  101  125   32  105  102  101  108  115  101  123
  32  101  120   99  104   32  103  101  116   32  101  120  101   99
  125   10  123   32  112  111  112   47  115  101  116  112   97  103
  101  100  101  118  105   99  101   32  119  104  101  114  101   10
  123   32  112  111  112   32   49   32  100  105   99  116   32  100
  117  112   32   47   80   97  103  101   83  105  122  101   32   80
  97  103  101   83  105  122  101   32  112  117  116   32  115  101
  116  112   97  103  101  100  101  118  105   99  101  125   10  123
  32   47  115  101  116  112   97  103  101   32  119  104  101  114
  101  123   32  112  111  112   32   80   97  103  101   83  105  122
  101   32   97  108  111   97  100   32  112  111  112   32  112   97
  103  101  112   97  114   97  109  115   32   51   32  123  101  120
  99  104   32  112  111  112  125   32  114  101  112  101   97  116
  10  115  101  116  112   97  103  101  125  105  102  125  105  102
  101  108  115  101  125  105  102  101  108  115  101  125  105  102
  101  108  115  101  125   32   98  105  110  100   32  100  101  102
  10   47   33  123   98  105  110  100   32  100  101  102  125   98
  105  110  100   32  100  101  102   47   35  123  108  111   97  100
  32  100  101  102  125   33   47   78   47   99  111  117  110  116
  116  111  109   97  114  107   32   35   10   47  114   71  123   51
  123   51   32   45   49   32  114  111  108  108   32   50   53   53
  32  100  105  118  125  114  101  112  101   97  116   32  115  101
  116  114  103   98   99  111  108  111  114  125   33   47   71  123
  50   53   53   32  100  105  118   32  115  101  116  103  114   97
  121  125   33   47   75  123   48   32   71  125   33   10   47  114
  54  123  100  117  112   32   51   32   45   49   32  114  111  108
  108   32  114   71  125   33   47  114   53  123  100  117  112   32
  51   32   49   32  114  111  108  108   32  114   71  125   33   47
  114   51  123  100  117  112   32  114   71  125   33   10   47  119
  47  115  101  116  108  105  110  101  119  105  100  116  104   32
  35   47   74   47  115  101  116  108  105  110  101   99   97  112
  32   35   10   47  106   47  115  101  116  108  105  110  101  106
  111  105  110   32   35   47   77   47  115  101  116  109  105  116
  101  114  108  105  109  105  116   32   35   47  100   47  115  101
  116  100   97  115  104   32   35   47  105   47  115  101  116  102
  108   97  116   32   35   10   47  109   47  109  111  118  101  116
  111   32   35   47  108   47  108  105  110  101  116  111   32   35
  47   99   47  114   99  117  114  118  101  116  111   32   35   10
  47  112  123   78   32   50   32  105  100  105  118  123   78   32
  45   50   32  114  111  108  108   32  114  108  105  110  101  116
  111  125  114  101  112  101   97  116  125   33   10   47   80  123
  78   32   48   32  103  116  123   78   32   45   50   32  114  111
  108  108   32  109  111  118  101  116  111   32  112  125  105  102
  125   33   10   47  104  123  112   32   99  108  111  115  101  112
  97  116  104  125   33   47   72  123   80   32   99  108  111  115
  101  112   97  116  104  125   33   10   47  108  120  123   48   32
  114  108  105  110  101  116  111  125   33   47  108  121  123   48
  32  101  120   99  104   32  114  108  105  110  101  116  111  125
  33   47  118  123   48   32   48   32   54   32   50   32  114  111
  108  108   32   99  125   33   47  121  123   50   32   99  111  112
  121   32   99  125   33   10   47  114  101  123   52   32   45   50
  32  114  111  108  108   32  109   32  101  120   99  104   32  100
  117  112   32  108  120   32  101  120   99  104   32  108  121   32
  110  101  103   32  108  120   32  104  125   33   10   47   94  123
  51   32  105  110  100  101  120   32  110  101  103   32   51   32
  105  110  100  101  120   32  110  101  103  125   33   10   47  102
  123   80   32  102  105  108  108  125   33   47  102   42  123   80
  32  101  111  102  105  108  108  125   33   47  115  123   72   32
  115  116  114  111  107  101  125   33   47   83  123   80   32  115
  116  114  111  107  101  125   33   10   47  113   47  103  115   97
  118  101   32   35   47   81   47  103  114  101  115  116  111  114
  101   32   35   47  114  102  123  114  101   32  102  105  108  108
  125   33   10   47   89  123   80   32   99  108  105  112   32  110
  101  119  112   97  116  104  125   33   47   89   42  123   80   32
  101  111   99  108  105  112   32  110  101  119  112   97  116  104
  125   33   47  114   89  123  114  101   32   89  125   33   10   47
  124   61  123  112  111  112   32  101  120   99  104   32   52   32
  49   32  114  111  108  108   32   49   32   97  114  114   97  121
  32   97  115  116  111  114  101   32   99  118  120   32   51   32
  97  114  114   97  121   32   97  115  116  111  114  101   32   99
  118  120   32  101  120   99  104   32   49   32  105  110  100  101
  120   32  100  101  102   32  101  120  101   99  125   33   10   47
  124  123  101  120   99  104   32  115  116  114  105  110  103   32
  114  101   97  100  115  116  114  105  110  103   32  124   61  125
  33   10   47   43  123  100  117  112   32  116  121  112  101   47
  110   97  109  101  116  121  112  101   32  101  113  123   50   32
  105  110  100  101  120   32   55   32   97  100  100   32   45   51
  32   98  105  116  115  104  105  102  116   32   50   32  105  110
  100  101  120   32  109  117  108  125  105  102  125   33   10   47
  64   47   99  117  114  114  101  110  116  102  105  108  101   32
  35   47   36  123   43   32   64   32  124  125   33   10   47   66
  123  123   50   32   99  111  112  121   32  115  116  114  105  110
  103  123  114  101   97  100  115  116  114  105  110  103   32  112
  111  112  125   97  108  111   97  100   32  112  111  112   32   52
  32   97  114  114   97  121   32   97  115  116  111  114  101   32
  99  118  120   10   51   32   49   32  114  111  108  108  125  114
  101  112  101   97  116   32  112  111  112   32  112  111  112   32
  116  114  117  101  125   33   10   47   73  120  123   91   49   32
  48   32   48   32   49   32   49   49   32   45   50   32  114  111
  108  108   32  101  120   99  104   32  110  101  103   32  101  120
  99  104   32  110  101  103   93  101  120   99  104  125   33   10
  47   44  123  116  114  117  101   32  101  120   99  104   32   73
  120   32  105  109   97  103  101  109   97  115  107  125   33   47
  73  102  123  102   97  108  115  101   32  101  120   99  104   32
  73  120   32  105  109   97  103  101  109   97  115  107  125   33
  47   73  123  101  120   99  104   32   73  120   32  105  109   97
  103  101  125   33   10   47   73   99  123  101  120   99  104   32
  73  120   32  102   97  108  115  101   32   51   32   99  111  108
  111  114  105  109   97  103  101  125   33   10   47   70  123   47
  67  111  108  117  109  110  115   32   99  111  117  110  116  116
  111  109   97  114  107   32   51   32   97  100  100   32   45   50
  32  114  111  108  108   47   82  111  119  115   32  101  120   99
  104   47   75   32   45   49   47   66  108   97   99  107   73  115
  49   32  116  114  117  101   62   62   10   47   67   67   73   84
  84   70   97  120   68  101   99  111  100  101   32  102  105  108
  116  101  114  125   33   47   70   88  123   60   60   47   69  110
  100   79  102   66  108  111   99  107   32  102   97  108  115  101
  32   70  125   33   10   47   88  123   47   65   83   67   73   73
  56   53   68  101   99  111  100  101   32  102  105  108  116  101
  114  125   33   47   64   88  123   64   32   88  125   33   47   38
  50  123   50   32  105  110  100  101  120   32   50   32  105  110
  100  101  120  125   33   10   47   64   70  123   64   32   38   50
  60   60   70  125   33   47   64   67  123   64   88   32   38   50
  32   70   88  125   33   10   47   36   88  123   43   32   64   88
  32  124  125   33   47   38   52  123   52   32  105  110  100  101
  120   32   52   32  105  110  100  101  120  125   33   47   36   70
  123   43   32   64   32   38   52   60   60   70   32  124  125   33
  47   36   67  123   43   32   64   88   32   38   52   32   70   88
  32  124  125   33   10   47   73   67  123   51   32   49   32  114
  111  108  108   32   49   48   32  100  105   99  116   32   98  101
  103  105  110   32   49  123   47   73  109   97  103  101   84  121
  112  101   47   73  110  116  101  114  112  111  108   97  116  101
  47   68  101   99  111  100  101   47   68   97  116   97   83  111
  117  114   99  101   10   47   73  109   97  103  101   77   97  116
  114  105  120   47   66  105  116  115   80  101  114   67  111  109
  112  111  110  101  110  116   47   72  101  105  103  104  116   47
  87  105  100  116  104  125  123  101  120   99  104   32  100  101
  102  125  102  111  114   97  108  108   10   99  117  114  114  101
  110  116  100  105   99  116   32  101  110  100   32  105  109   97
  103  101  125   33   10   47  126  123   64   32  114  101   97  100
  32  123  112  111  112  125   32  105  102  125   33   10  101  110
  100   32  100  101  102   10   37   37   69  110  100   82  101  115
  111  117  114   99  101   10   47  112   97  103  101  115   97  118
  101   32  110  117  108  108   32  100  101  102   10   37   37   69
  110  100   80  114  111  108  111  103   10   37   37   80   97  103
  101   58   32   49   32   49   10   37   37   37   37   66  101  103
  105  110   80   97  103  101   83  101  116  117  112   10   71   83
  95  101  112  115  119  114  105  116  101   95   51   95   48   95
  49   48   48   49   32   98  101  103  105  110   10   47  112   97
  103  101  115   97  118  101   32  115   97  118  101   32  115  116
  111  114  101   32   49   57   55   32  100  105   99  116   32   98
  101  103  105  110   10   48   46   48   50   54   55   56   53   55
  32   48   46   48   50   54   55   56   53   55   32  115   99   97
  108  101   10   37   37   69  110  100   80   97  103  101   83  101
  116  117  112   10  103  115   97  118  101   32  109   97  114  107
  10   81   32  113   10   48   32   48   32   53   56   48   54   48
  56   32   48   32   48   32   53   56   48   54   48   56   32   94
  32   89   10   75   10   54   56   49   32   53   56   48   49   53
  56   32   50   49   52   32   51   49   52   32   64   67   10   44
  10   49   68  117   79   50   53  109   38  108   34   37   63   81
  50   70   37   91   92   67   38   48   83   65   39  104   81  112
  76   66   60   75   65   37   75   81  110   34   62   54   71   79
  62  106   80   33   73   51   63   71  109  104  108   35   49   95
  114  110  103   59   90  109   72  110   45   84  111  107  115   89
  82  110   35   81   69   33   10  109   73   49   38   51   58   37
  45  115   53  114   86   97   64  115   93   62   41   44   75  113
  115   78  113   65  104  110   73  116   57  114   86   99   48   63
  109  115  107   59   74   94   65   73  111   97   74   43   37  106
  66   94   92  116  110   68   72  105   68  113   34   94   65   68
  106   75  103   77   81  114  105   68   54   78   93   79   10   36
  90   49  116   39  104   61  112   88   67   73  114   53   53   91
  68  104   37   89   46  112   92   43   87   58   68   63   37   60
  60   73  116   41   92   58  112   65   93   86   90  115   55   94
  71   93  113  113  104   41   65  112   79   64   91   66   71   80
  59   37   57   73  114   70  102   77   73   75   39   55   96  115
  56   82   57   65   10  115   53   47   49   88  115   42   97  107
  67  113  102   96   35   46  104  103   94   43   52  112   65   80
  36   75   68  114   58   78   68   94   79   72   42   56   66   94
  96   89   51   44   68   87   71   85  112   65   59   61   39   68
  115   84   93  115   81   49   42   73   87   37   65  104   94   42
  103   84   64  111   78   64   75   33   82   57   10   95   98   52
  69  110  112   41   55   52  107   81   44   61   37   95   89   91
  75   46   36   48   85   72   59   88   75   78   65   83   58   63
  117   39   50   44   62   87   39   68   43  105   74   34   76   69
  69   43   36   49   48   61   33   96   67   69   90   53   42  106
  103  112   89   89   96   38  112   89   89   96   38  112   89   89
  96   38   10  112   89   89   96   38  112   89   89   96   38  112
  89   89   96   38  112   89   89   96   38  112   89   89   96   38
  112   89   89   96   38  112   89   89   96   38   94   89  102   39
  64  101  116   36   51   83   90   53   90   91  111   36   87   43
  56   46  115   52  100   80   66  110  100   35   67   51   90  106
  78   35   47   35   46   96   84   97   10   90   63   65  111   52
  126   62   10   57   50   52   32   53   56   48   49   53   56   32
  49   57   57   32   50   48   49   32   64   67   10   44   10   46
  85   95   70   75   33  108  117   75   35   52   35  103  111   58
  84   80   61   84   62   35   84   43   93   67   34  113   89   97
  43   36   44   82   44   86   35   86   95  101   82   83   48   65
  104   65   37   49  108   65  113   53   96   33   53   38   74   72
  76   33   34   40  104  106   51   60   43   68  104   56   58   44
  84   40   56   56   10   85   96   50  110  105   35   49   64   67
  81   37  114   82   74   71   45  100   90   94   74   80   67   37
  68   64   61   47   94   37   84   80   55   57   49   83   91  110
  52   33  109   41  114   38  104  107   60  110   52  108   66  111
  109   81   94   99  101   91   57  102   97  104   44  114   82   76
  61   49  114   80   48   88   42   60   67   98   10  115   42   78
  62  113  113   82  117   99   84  114   69  111   71   77   94   93
  47   75   67  115   56   68  111   78  115   56   84   82   91   68
  117   84  100   70  115   49  101   84  103   74   44  100   45   47
  108  102   35   37   46   94   65   73  115   33   94   93   47   51
  75  115   56   77   96  108  112   79   64   44   93   74   40  114
  108   84   10  114  113  108   47   54  110   37   83   80  112   94
  38   78   83  106  115   55   95   34   93  113  113  104   62   72
  112   78   81   78   54  104   62   90  108   87  112   92   83  110
  43  104   89   89   37   97  104  115   74   50   73   72  103   94
  86   45   71   53   75   35   78  104   55   42   63  115  112   86
  45   85   63  103   52   45   84  100   10  109   91   95   43   64
  113  101   61  103   81   91  101  107   56   99   68   62   41   81
  79   38   57  104  117   60   70   83   61   61  113   61   41   69
  58  117   52   98  103   85   46   58   77  104   96   60   93   74
  46  101   36   86   34   80   94   36   54   88   98   60   78   45
  68   63   47   44   67   41   67   65   37  101   85  110   33   87
  10   58  114  109   55   66   34  112  117   51   35   48   91  113
  114   97   39   74   81   50  115   75   42  108  100   48   36  114
  52   97   87  104  117  126   62   10   49   49   52   54   32   53
  56   48   49   53   56   32   51   54   48   32   50   48   49   32
  64   67   10   44   10   44   34   34   70  106   39   91   43   68
  49   33  115  102  100  116   84   74  114   79   92   36  107   80
  72   47   48   83   63   54  105   54   46   56   76   77  108   48
  55  105  101   72   97   39   45  105   35   52  114   67  115   74
  105   92   78   42   99  112   62   59   57   41  114   55   72   50
  107  109   37   36   99   73  101   78   48  110   10   50  117   71
  87   45   73   67   74   38   42   68  117   53   37   65   68   54
  38   48   64   94   93   51   39   67   67   62   93   36  105  104
  116   80  108   76   74   41   58   51   84  104  101  104   99   91
  113  116  107  109   65   92   36  117   56   39  109   74  108  110
  55  104   62   59   93  114  114   86   34  117  111  114  114   57
  97  102   10  112   62   52   88   42  113  103   92   62   53  104
  113  114   65   79  104  113   82  107   94  104  113   83   36   88
  115   41   37   96   46  103   93   46   60   46   68   59   53   42
  72   89   66   74   78   70   68   61   46   65  106  115   41   45
  96  111  109   67   51   43   70   73   68   52   86   61   72   91
  35   74   72  112   64  110   61   92   10  103   79   74   86  106
  103   91   53   37   46  109   74  108   83   82   68   86  116   75
  93   92   44   80   58   96  114   83   63   65  102  115   49   97
  33   62  104   90   40   63   61  104   61  111  104   44   73   71
  79   89   84  113  113   76  108   62  104  116  117   49   38  107
  54  104   42   97  114   69  101   66   46  114  114   59  114   79
  10  109   74   67  105   36  114   83   82   86   66  109   73   48
  99   73  104  117   60   77   94  114  112   66   72  110  104   88
  67   70   65   94   89   82  100  115  114   83   80   39   60   72
  105   78   79   52  104   62   91   69   66   43  100   62   40   95
  73   75   38  115   59  104   86   89  106   80   90   50   60   47
  64   89   74   47  108   78   10   91  114   54   51   33  104   86
  91   86  113  112   85   60  108   73  112   61   54   83   45  104
  86   92   38   35  108   74   52   66  110  115   49   81   47   39
  103   46  101   70   46   93  116   87   64   91  104  116   64  115
  95   86   47   68  100   84   73  111   98  103   70   91  110   74
  87   71   66  108   61   73   74  112   63   64   60  113   10   37
  53   42  105   89   50   79   49  116   50   76   42   40   60   35
  66  101   91  117  104   54   38   40   82   52   46  113  112   92
  79   34  113   60   39  110   36  111   33  107   82   45   35   61
  106   67   75   83   39   73   65   89  103   78  114   50   69   47
  66   90   47   38   81  103   95   88   49   41   89   89   57   97
  67   89   88   88   10   54   61   76   76   56   39  113  105  111
  103   99  110   76   40   90   95   64   46   80  113   54   79   52
  77  112   75  106   98   47  107   40   97  114  102   53   83   50
  71  107   76   75   42   54   67   35   49  110   67   69   78  102
  100  109   86   86   34   58   81   60   56   75   43   74  126   62
  10   49   53   50   56   32   53   56   48   49   53   56   32   50
  50   55   32   50   48   49   32   64   67   10   44   10   52   38
  45   48   83   76   41  116   77   39   65  102   92   91   35   83
  93  105  113   53   61   67  106   99   35   35   87   78  114   56
  35   55   42   35   57   75   71   91   71  113   67   33   94   57
  74   54  111   46   65   73   74  114   46   38   94   80   91   36
  36   78   41  104   92   74   57   56   50  105  103   58   66  102
  65   82   58   10  100   76   70   86   54  101   39   76   67  102
  83   35   34   99   73   95  101   83   33   83  108   48   49   43
  56   56   50  110   59   35   62   71   85   41   39   91   59   77
  49   88   60  113   99   69   43  108   49   88   69   34  112   64
  100  100  108  108   47   50   53   58   73  102   70   99  108  100
  88   49   66   93   73  101   86   91  117   10  112   90  108   92
  106  115   56   77  111   49   94   92   64   75  107  113  117   53
  73   80   94   91   77   38   76  115   55   90   62   58   74   44
  102   54   34  115   54  102  111   40   88   55   44   96  117   97
  41   49  104   85   93  116   97   37   53  103   93   37   53  100
  94   91   76   85  102  114  114   50  111   81  114   86  117   57
  62   10  115   56   45   91   53  112   65   87  116   58  115   54
  92   83   64  109   98   64  100   56  110   37   92   56  104  112
  64  110   85   33   93   68  109   60   73   69   51   56   66   62
  58   93   57   48  112  104   61  112   66   81  109   73  112   77
  85  112   91   45  105   51  114  113   63   60   88  113  116  111
  103   85  112   79   51   35   46   10  113  116  111   55   69  109
  73   40   34  102  109  102   48  111   46  115   42   74   92   96
  109   74  108   37  116  113  115   79   75  116  110   44   36  107
  101  109  115  106   54   76  109   98   76   44   41   73   71   58
  112   93  113  108   92   90   47   92  112   74  112   56  102   53
  50  110   69  101   86   57   58   63   66  103   37   41   64   10
  88   59   70   79  113   59   99   40  104   85  100  103   63  112
  87   95   42   92   75   58   87   42  108   58   55   57   35   44
  90  126   62   10   49   55   55   55   32   53   56   48   49   53
  56   32   49   51   53   32   50   56   51   32   64   67   10   44
  10   46   89   42   90  104   36   54   89   45   93   41   49  105
  62   48   51   88  117   94   67  105   60   77   97   89   83   94
  56   84   96   66   58  115  103  101   54   43   84   64   59   58
  117   54  104   73   68   78  102   72   44   60  114   55   33   46
  109   70   65  114   44   62   79   85   62   54  101   92   59   33
  115   70   96   53   36   53   10  104  103   88   71   62  104  102
  38   74   45  113  110   39   63   58  104   86   91   47   72  104
  86   91   52   59  113  101  117   71   70  113  110   69   42   66
  72  105   42   93   80  109   94  114   48   45   97   43   41  117
  114  104  103   97   79   53  104  103   97   79   53  104  103   97
  79   53  104  103   97   79   53  104  103   97   79   53   10  104
  103   97   79   53  104  103   97   79   53  104  103   97   79   53
  104  103   97   79   53  104  103   97   79   53  104  103   97   79
  53  104  103   97   79   53  104  103   97   79   52   67   95   36
  95   96   54   46   61   96   75  109  111   80   44   75   60   94
  83   92   63   74   44   61   96  109   74   44   61   96   97  104
  115   93   73  107   10  104  115   93   73  107  104  115   93   73
  107  104  115   93   73  107  104  115   93   73  107  113  115   88
  70   78  113  115   88   70   94   73  115   94   74   83   88   91
  62  113   37   75   42  104  126   62   10   49   57   51   52   32
  53   56   48   49   53   56   32   49   49   55   32   50   57   55
  32   64   67   10   44   10   44   98   97   60  106   41  111   50
  87   48   42   36   70   72   41   58   52  111   72   52   50   71
  99   70   63  108   51  112   80  112  107  109   36   66   50   58
  117   55   92   66   72  111   70   88   50  111  108   37   60   68
  110   40   88  109  107  114   86   35   79   82   94   65   89  102
  112  114   85   39   82   88  112   85   66  109   64   10  104  113
  83   46  117  110   44   57   95   70  104   86   83   58   71   94
  38   78   71   43  113  110   39   87   59  104   55  112   89   91
  112   92   41   38   71   97   43   33  112   91  104   61  112   66
  81   73   71   93   77   78  112   65   79   97   83   94   77  105
  34   89  113  116   67   33   95  110   37   87  101  117   73  102
  36  107   88   10  107   54  103   74   71  104  113  114  107   60
  114  114   50   87   99  114   85   39   40   78  103   89   86  104
  114   94   37   94   69   50  103   89   95   98   55   94   86   57
  48   75   94   86   57   48   78  114  113  106   71   44  113  115
  70   64   76  101   92   67  104   84   70  104   91  103   99   92
  42  103   66  108  100   94   82  107   79   10   68   47   59   56
  65   81   37  107   81   86   57   35   54   49   94   60   38   74
  68   45   58  116   46   49  109   74  113   70   52   87   33   84
  88   52   91  115   56   87   45   33  115   54   34   44   35   74
  100   50   81   66   37   92   51  105   35   70   83   66   41   48
  114   86   99   87  110   73  115  107   89  115   88   94   57   76
  55   10   64   54  112  104   71   58   93  126   62   10   99  108
  101   97  114  116  111  109   97  114  107   32  101  110  100   32
  101  110  100   32  112   97  103  101  115   97  118  101   32  114
  101  115  116  111  114  101   10   32  115  104  111  119  112   97
  103  101   10   37   37   80   97  103  101   84  114   97  105  108
  101  114   10   37   37   84  114   97  105  108  101  114   10   37
  37   80   97  103  101  115   58   32   49   10   37   37   66  111
  117  110  100  105  110  103   66  111  120   58   32   49   56   32
  49   53   53   51   57   32   53   53   32   49   53   53   52   57
  10   37   37   72  105   82  101  115   66  111  117  110  100  105
  110  103   66  111  120   58   32   49   56   46   50   52   49   48
  55   50   32   49   53   53   51   57   46   57   52   54   57   53
  56   32   53   52   46   57   51   55   53   48   50   32   49   53
  53   52   56   46   51   53   55   54   55   50   10   37   37   76
  97  116  101  120   87  105  100  116  104   58   32   51   56   46
  49   52   56   51   50   10   37   37   76   97  116  101  120   72
  101  105  103  104  116   58   32   56   46   51   51   51   51   49
  10   37   37   76   97  116  101  120   68  101  112  116  104   58
  32   48   46   48   10   37   37   69   79   70
  > def
} moduledef
