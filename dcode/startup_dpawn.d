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

|======================== startup_dnode.d ================================

| Contains procedures for:
|  - inspection of objects
|  - object/text interconversion
|  - transcription of objects
|  - file <=> VM interchange
|  - module support
|  - emulators of dvt operators

/dm_type /dpawn def

save /startup_common_save name 
/startup_common_buf vmstatus sub 10 div /b array def
startup_common_save capsave {
  getstartupdir (startup_common.d) startup_common_buf readfile mkact exec
  getstartupdir (startup_libs.d) startup_common_buf readfile mkact exec
} stopped startup_common_save restore {
  1024 /b array 0 
  (Unable to load: ) fax
  getstartupdir fax (startup_common.d\n) fax
  0 exch getinterval toconsole
  stop
} if

| pid rank (source) code
/error_ops_length 4 def

| -- | pid rank true
/_makeerror {
  getpid unpid
  mpirank
  true
} bind def

|============================= userdict =================================

|==================================================== dnode communications
| save executable | ??
/dnodereceive {
  exch dup capsave userdict /dnodesave put
  ~unlock stopped pop
  userdict /dnodesave get restore 
  dnoderespond
} bind def

/dnoderespond {
  save
  ~[~[mpirank ~mpidata ~begin ~makeready_dpawn] ~lock] rsend
  restore
} bind def

| save {} | --
/wait_dnode {
  {
    1 index capsave 
    ~unlock stopped
    save ~[~[mpirank ~mpidata ~begin ~notify] ~lock] rsend restore
    exch restore ~stop if
  } lock
} bind def

| -- | stopped
/kickdnode {
  {
    save ~[~[mpirank ~mpidata ~begin ~notify] ~lock] rsend restore
    ~halt stopped
  } lock
} bind def

| save null | --
/recv_kick_dnode {
  {continue pop restore} lock
} bind def

/recv_module {
  dup /myName get module 3 -1 roll transcribe _module
} bind def

|================== recv_transcribe ====================
| anything | anything-transcribed
|
| copies anything by transcribing if anything is composite.
| otherwise, leaves it alone
|
/transcribedict [
  {/arrayclass /listclass /dictclass} {~transcribe bind} forall
  {/nullclass /numclass /opclass /nameclass /boolclass /markclass} {
    null mkact
  } forall
] makestruct def

/recv_transcribe {
  transcribedict 1 index class get exec
} bind def

|------------------------------------------------ whoami
| prints name and port
| for use with make_toconsole to identify who looks like who
| -- | -- <<printout>>
|
/whoami ~[
  1024 /b array 0 * mpirank * number (\n) fax 0 exch getinterval
  ~toconsole
] bind def

(Starting...\n) toconsole
{
  /reqfiles [
    (matrix.d)
  ] def
  /optfiles [
    /ENABLE_PETSC {
      {getstartupdir (petsc.d)}
    } if_compile
    {getconfdir (dpawn.d)} 
    {gethomedir (.dpawn)}
  ] def
} {
  reqfiles ~loadstartup forall
  optfiles {exec loadopt} forall
} incapsave
