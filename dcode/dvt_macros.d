| -*- mode: d; -*-
| Copyright 2011 Alexander Peyser & Wolfgang Nonner
|
| This file is part of Deuterostome.
|
| This program is free software: you can redistribute it and/or modify
| it under the terms of the GNU General Public License as published by
| the Free Software Foundation, either version 2 of the License, or
| (at your option) any later version.
|
| This program is distributed in the hope that it will be useful,
| but WITHOUT ANY WARRANTY; without even the implied warranty of
| MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
| GNU General Public License for more details.
|
| You should have received a copy of the GNU General Public License
| along with this program.  If not, see <http://www.gnu.org/licenses/>.
|------------------------------ DVT macros ---------------------------

50 dict dup begin |[
  /myname (DVT macros) def
  /myshortname myname def
  /keywords {
    (Files:) PRE     (  MakeDir) KEY 
                     (  Remove) KEY
                     (  Copy) KEY 
                     (  Save) KEY 
                     (  Load) KEY 
                     (  Open) KEY NL
    11 GAP () PRE    (Lib) KEY 
                     (  Rsync) KEY 
                     (  Rsync-del) KEY NL   
    (Dnode:) PRE     (  Setup) KEY 
                     (  Connect) KEY 
                     (  Talk) KEY NL
    15 GAP () PRE    (Shrink) KEY 
                     (  Disconnect) KEY 
                     (  Abort) KEY NL
    /ENABLE_PERL {
    (PrintFrom:) PRE (  Talk) KEY 
                     (  Ascii) KEY 
                     (  PS) KEY
                     (  EPS) KEY
      /ENABLE_XPS {
                     ( xPS) KEY
      } {
                     ( x1PS) KEY 
                     ( x2PS) KEY
      } ifelse_compile
                     ( pdf) KEY 
                     ( dvi) KEY 
                     ( tex) KEY NL      
    5 GAP (PrintTo:) PRE 
                     (  gs) KEY 
                     (  paper) KEY 
                     (  pdf) KEY 
                     (  dvi) KEY NL
    } if_compile
    (Selection:) PRE (  show) KEY NL
    (Action:) PRE    (  f1-send) KEY 
                     (  f2-cont) KEY
                     (  f3-stop) KEY 
                     (  f4-abort) KEY
                     (  ctrl-!-scream) KEY
    10 GAP
  } def

  /commands [
|-- Files
    (@\(mkdir \) fax faxLpage\n)
    (@\(rm -rf \) fax faxRpage\n)
    (@\(cp -R \) fax faxRpage \( \) fax faxLpage\n)
    (getRpage getLpage tofiles\n)
    (getRpage dup 0 get exch 1 get { exch dup 3 -1 roll fromfiles } forall pop\n)
    (@\(emacsclient -n -s \) fax emacs_server_name fax \( \) fax faxRpage\n)
    (getRpage dup 0 get exch 1 get {exch dup 3 -1 roll loadlib pop} forall pop\n)
    (@\(rsync -av \) fax faxRpage \(/. \) fax faxLpage \(/.\) fax\n)
    (@\(rsync -av --delete \) fax faxRpage \(/. \) fax faxLpage \(/.\) fax\n)
|-- Dnodes
    (% dvtsup begin dnode_resize end\n)
    (% dvtsup begin ~_cx ~_ccx dnode_up end\n)
    (% dvtsup begin ~_c ~_cc dnode_up end\n)
    (% dvtsup begin {knode _kill} fornodes end\n)
    (% dvtsup begin {knode _dx} fornodes end\n)
    (% dvtsup begin {knode _sendabort} fornodes end\n)
    /ENABLE_PERL {
      |-- PrintFrom
      (@getstartupdir fax \(print.pl talk \) fax )
      (@getstartupdir fax \(print.pl ascii \) fax )
      (@getstartupdir fax \(print.pl ps \) fax )
      (@getstartupdir fax \(print.pl eps \) fax )
      /ENABLE_XPS {
        (@getstartupdir fax \(print.pl x2ps \) fax )
      } {
        (@getstartupdir fax \(print.pl x1ps \) fax )
        (@getstartupdir fax \(print.pl x2ps \) fax )
      } ifelse_compile
      (@getstartupdir fax \(print.pl pdf \) fax )
      (@getstartupdir fax \(print.pl dvi \) fax )
      (@getstartupdir fax \(print.pl tex \) fax )
      |-- PrintTo
      (\(gs \) fax faxRpage\n)
      (\(lw \) fax faxRpage\n)
      (\(xpdf \) fax faxRpage\n)
      (\(xdvi \) fax faxRpage\n)
    } if_compile
    (save 1024 /b array 0 faxLpage \(:\) fax faxRpage \(\\n\) fax 0 exch getinterval toconsole restore\n)
    {userdict begin F1_KEY}
    {userdict begin F2_KEY}
    {userdict begin F3_KEY}
    {userdict begin F4_KEY}
    {userdict begin CTRL_1_KEY}
  ] def |]
end makemacros
