#ifndef DM_MPI_H
#define DM_MPI_H

#include "dm.h"
#include <mpi.h>

#define MPI_NOMSG (MPI_ERRS+0) /* no event available */

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
