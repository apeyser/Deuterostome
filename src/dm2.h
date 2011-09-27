/*

Copyright 2011 Alexander Peyser & Wolfgang Nonner

This file is part of Deuterostome.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/
#ifndef DM_2_H
#define DM_2_H

#include "dm.h"

typedef P (*SourceFunc)(B* buffer, P size);

DLL_SCOPE P fromsource(B* bufferf, SourceFunc r1, SourceFunc r2);
DLL_SCOPE P tosource(B* rootf, BOOLEAN mksave, SourceFunc w1, SourceFunc w2);

DLL_SCOPE P makeDmemory(LBIG specs[5]);
DLL_SCOPE P wrap_hi(B* hival);
DLL_SCOPE P wrap_libnum(UP libnum);
DLL_SCOPE B* nextlib(B* frame);
DLL_SCOPE B* geterror(P e);
DLL_SCOPE B *makedict(L32 n);
DLL_SCOPE void cleardict(B *dict);
DLL_SCOPE B *makeopdictbase(B *opdefs, P *errc, B **errm, L32 n1);
DLL_SCOPE B *makeopdict(B *opdefs, P *errc, B **errm);
DLL_SCOPE void d_reloc(B *dict, P oldd, P newd);
DLL_SCOPE void d_rreloc(B *dict, P oldd, P newd);
DM_HOT DLL_SCOPE void makename(B *namestring, B *nameframe);
DM_HOT DLL_SCOPE void pullname(B *nameframe, B *namestring);
DM_HOT DLL_SCOPE P compname(B *nameframe1, B *nameframe2);
DM_HOT DLL_SCOPE BOOLEAN matchname(B *nameframe1, B *nameframe2);
DM_HOT DLL_SCOPE B *lookup(B *nameframe, B *dict);
DM_HOT DLL_SCOPE BOOLEAN insert(B *nameframe, B *dict, B *framedef);
DLL_SCOPE BOOLEAN mergedict(B *socket, B *sink);
DLL_SCOPE P (*execfd_func)(void);
DM_HOT DLL_SCOPE P exec(UL32 turns);
DM_HOT DLL_SCOPE P exec_step(void);
DLL_SCOPE P foldobj(B *frame);
DLL_SCOPE P transcribe(B* frame);
DLL_SCOPE P unfoldobj(B *frame, P base, B isnonnative);
DLL_SCOPE P foldobj_ext(B* frame);
DLL_SCOPE BOOLEAN foldobj_mem(B** base, B** top);
DLL_SCOPE void foldobj_free(void);
DLL_SCOPE P deendian_frame(B *frame, B isnonnative);
//L deendian_array(B* frame, B isnonnative);
//L deendian_list(B* frame, B isnonnative);
//L deendian_dict(B* dict, B isnonnative);
//L deendian_entries(B* doct, B isnonnative);

DLL_SCOPE void setupdirs(void);
DLL_SCOPE P op_getstartupdir(void);
DLL_SCOPE P op_getconfdir(void);
DLL_SCOPE P op_gethomedir(void);
DLL_SCOPE P op_getmyport(void);
DLL_SCOPE P op_getplugindir(void);
DLL_SCOPE P op_getexecdir(void);

DLL_SCOPE P op_getpid(void);
DLL_SCOPE P op_unpid(void);
DLL_SCOPE P op_makepid(void);

DLL_SCOPE P op_syshi(void);
DLL_SCOPE P op_syslibnum(void);

DLL_SCOPE P op_aborted(void);

// this needs to be called before any other onexit handlers are set.
DLL_SCOPE void setuphandlers(void);

DLL_SCOPE P int_repush_stop(P (*abortfunc)(void));
#define repush_stop() int_repush_stop(op_abort)

DLL_SCOPE void createfds(void);

DLL_SCOPE P op_getmyname(void);
DLL_SCOPE P op_getmyfqdn(void);

#endif //DM_2_H
