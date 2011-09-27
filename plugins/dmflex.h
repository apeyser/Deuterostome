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
#ifndef DMFLEX_H
#define DMFLEX_H

#define PLUGIN_NAME dmflex
#include "../src/plugin.h"

#define op_makestream  EXPORTNAME(op_makestream)
#define op_writestream EXPORTNAME(op_writestream)
#define op_readstream  EXPORTNAME(op_readstream)
#define op_closestream EXPORTNAME(op_closestream)
#define op_execstream  EXPORTNAME(op_execstream)
#define op_seekstream  EXPORTNAME(op_seekstream)

#define x_op_execstream      PRIVATENAME(x_op_execstream)
#define x_op_execstream_name PRIVATENAME(x_op_execstream_name)

// (dir) (file) flags | handle
// where flags is one of /r /w /rw /rwtrunc  /a
P op_makestream(void);

// handle position offset | handle
// where position is /s --> beginning or /e --> end or /c --> current
P op_seekstream(void);

// handle (string) | handle
P op_writestream(void);

// handle (string) offset bytes/* | handle (string) offset true/false
P op_readstream(void);

// handle | --
P op_closestream(void);

// handle | handle (something executed)
P op_execstream(void);

P x_op_execstream(void);
const char* x_op_execstream_name;

#define DMFLEX_STATE_ERR 0x01L //Illegal state for operation
#define DMFLEX_CLOSE_ERR 0x02L //Already closed
#define DMFLEX_FLAGS     0x03L //Incorrect flags 
#define DMFLEX_SEEK      0x04L //Illegal seek name


#define DMFLEX_HANDLE "DMFLEX"

#define DMFLEX_FD(frame) (LONGBIG_VAL(OPAQUE_MEM(frame, DMFLEX_FD_N)))
#define DMFLEX_STATE(frame) (LONGBIG_VAL(OPAQUE_MEM(frame, DMFLEX_STATE_N)))
#define DMFLEX_CHBUF(frame) (VALUE_PTR(OPAQUE_MEM(frame, DMFLEX_CHBUF_N)))
#define DMFLEX_BUFSIZE(frame) (ARRAY_SIZE(OPAQUE_MEM(frame, DMFLEX_CHBUF_N)))
#define DMFLEX_BUFPOS(frame) (VALUE_PTR(OPAQUE_MEM(frame, DMFLEX_BUFPOS_N)))
#define DMFLEX_NCHARS(frame) (ARRAY_SIZE(OPAQUE_MEM(frame, DMFLEX_BUFPOS_N)))
#define DMFLEX_ATBOL(frame) (LONGBIG_VAL(OPAQUE_MEM(frame, DMFLEX_ATBOL_N)))
#define DMFLEX_BUFSTAT(frame) (LONGBIG_VAL(OPAQUE_MEM(frame, DMFLEX_BUFSTAT_N)))
#define DMFLEX_PARENT(frame) (VALUE_PTR(OPAQUE_MEM(frame, DMFLEX_PARENT_N)))

#define DMFLEX_OPEN (1)
#define DMFLEX_EXEC (2)
#define DMFLEX_CLOSED (0)
#define DMFLEX_ERR (3)
#define DMFLEX_EOF (4)

#endif
