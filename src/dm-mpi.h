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
#ifndef DM_MPI_H
#define DM_MPI_H

#include "dm.h"
#include <mpi.h>

DLL_SCOPE P mpiiprobe(MPI_Comm comm, P* tag, P* rank, P* count);
DLL_SCOPE P mpiprobe(MPI_Comm comm, P* tag, P* rank, P* count);
DLL_SCOPE P mpirecv(MPI_Comm comm, P tag, P rank, B* buffer, P size);
DLL_SCOPE P mpisend(MPI_Comm comm, P tag, P rank, B* buffer, P size);
DLL_SCOPE P mpibroadcast(MPI_Comm comm, P rank, B* buffer, P size);
DLL_SCOPE P mpibarrier(MPI_Comm comm);

DLL_SCOPE MPI_Comm getworldcomm(void);
DLL_SCOPE P getworldsize(void);
DLL_SCOPE P getworldrank(void);
DLL_SCOPE MPI_Comm getparentcomm(void);

DLL_SCOPE void initmpi(void);

#endif
