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
#include <src/mat/impls/dense/mpi/mpidense.h>
#include <src/mat/impls/aij/mpi/mpiaij.h>
//#include <stdio.h>

PetscErrorCode dm_MatTranspose_MPIDense(Mat A,Mat *matout)
{ 
  Mat_MPIDense   *a = (Mat_MPIDense*)A->data;
  Mat_SeqDense   *Aloc = (Mat_SeqDense*)a->A->data;
  Mat            B;
  PetscInt       M = A->rmap.N,N = A->cmap.N,m,n,*rwork,rstart = A->rmap.rstart;
  PetscErrorCode ierr;
  PetscInt       j,i;
  PetscScalar    *v;

  PetscFunctionBegin;
  if (!matout && M != N) {
    SETERRQ(PETSC_ERR_SUP,"Supports square matrix only in-place");
  }
  ierr = MatCreate(A->comm,&B);CHKERRQ(ierr);
  ierr = MatSetSizes(B, A->cmap.n,A->rmap.n,N,M);CHKERRQ(ierr);
  ierr = MatSetType(B,A->type_name);CHKERRQ(ierr);
  ierr = MatMPIDenseSetPreallocation(B,PETSC_NULL);CHKERRQ(ierr);

  m = a->A->rmap.n; n = a->A->cmap.n; v = Aloc->v;
  ierr = PetscMalloc(m*sizeof(PetscInt),&rwork);CHKERRQ(ierr);
  for (i=0; i<m; i++) rwork[i] = rstart + i;
  for (j=0; j<n; j++) {
    ierr = MatSetValues(B,1,&j,m,rwork,v,INSERT_VALUES);CHKERRQ(ierr);
    v   += m;
  } 
  ierr = PetscFree(rwork);CHKERRQ(ierr);
  ierr = MatAssemblyBegin(B,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(B,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  if (matout) {
    *matout = B;
  } else {
    ierr = MatHeaderCopy(A,B);CHKERRQ(ierr);
  }
  PetscFunctionReturn(0);
}

PetscErrorCode dm_MatTranspose_MPIAIJ(Mat A,Mat *matout)
{ 
  Mat_MPIAIJ     *a = (Mat_MPIAIJ*)A->data;
  Mat_SeqAIJ     *Aloc=(Mat_SeqAIJ*)a->A->data,*Bloc=(Mat_SeqAIJ*)a->B->data;
  PetscErrorCode ierr;
  PetscInt       M = A->rmap.N,N = A->cmap.N,ma,na,mb,*ai,*aj,*bi,*bj,row,*cols,i,*d_nnz;
  PetscInt       cstart=A->cmap.rstart,ncol;
  Mat            B;
  PetscScalar    *array;

  PetscFunctionBegin;
  if (!matout && M != N) SETERRQ(PETSC_ERR_ARG_SIZ,"Square matrix only for in-place");

  /* compute d_nnz for preallocation; o_nnz is approximated by d_nnz to avoid communication */
  ma = A->rmap.n; na = A->cmap.n; mb = a->B->rmap.n;
  ai = Aloc->i; aj = Aloc->j; 
  bi = Bloc->i; bj = Bloc->j; 
  ierr = PetscMalloc((1+na+bi[mb])*sizeof(PetscInt),&d_nnz);CHKERRQ(ierr);
  cols = d_nnz + na + 1; /* work space to be used by B part */
  ierr = PetscMemzero(d_nnz,(1+na)*sizeof(PetscInt));CHKERRQ(ierr);
  for (i=0; i<ai[ma]; i++){
    d_nnz[aj[i]] ++;  
    aj[i] += cstart; /* global col index to be used by MatSetValues() */
  }

  ierr = MatCreate(A->comm,&B);CHKERRQ(ierr);
  ierr = MatSetSizes(B,A->cmap.n,A->rmap.n,N,M);CHKERRQ(ierr);
  ierr = MatSetType(B,A->type_name);CHKERRQ(ierr);
  //ierr = MatMPIAIJSetPreallocation(B,0,d_nnz,0,d_nnz);CHKERRQ(ierr);
  ierr = MatMPIAIJSetPreallocation(B,0,d_nnz,M-A->rmap.n,PETSC_NULL);CHKERRQ(ierr);
    
  ierr = MatSetOption(B, MAT_COLUMNS_SORTED);CHKERRQ(ierr);
  ierr = MatSetOption(B, MAT_ROWS_SORTED); CHKERRQ(ierr);
  //  ierr = MatSetOption(B, MAT_COLUMN_ORIENTED); CHKERRQ(ierr);

  /* copy over the A part */ 
  array = Aloc->a;
  row = A->rmap.rstart;
  for (i=0; i<ma; i++) {
    //fprintf(stderr, "A: %i\n", (int) row);
    ncol = ai[i+1]-ai[i];
    ierr = MatSetValues(B,ncol,aj,1,&row,array,INSERT_VALUES);CHKERRQ(ierr);
    row++; array += ncol; aj += ncol;
  } 
  aj = Aloc->j;
  for (i=0; i<ai[ma]; i++) aj[i] -= cstart; /* resume local col index */

  /* copy over the B part */
  array = Bloc->a;
  row = A->rmap.rstart; 
  for (i=0; i<bi[mb]; i++) {cols[i] = a->garray[bj[i]];}
  for (i=0; i<mb; i++) {
    //fprintf(stderr, "A: %i\n", (int) row);
    ncol = bi[i+1]-bi[i];
    ierr = MatSetValues(B,ncol,cols,1,&row,array,INSERT_VALUES);CHKERRQ(ierr);
    row++; array += ncol; cols += ncol;
  } 
  ierr = PetscFree(d_nnz);CHKERRQ(ierr);
  //fprintf(stderr, "MatAssemblyBegin\n");
  ierr = MatAssemblyBegin(B,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  //fprintf(stderr, "MatAssemblyEnd\n");
  ierr = MatAssemblyEnd(B,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);

  if (matout) {
    *matout = B;
  } else {
    //fprintf(stderr, "MatHeaderCopy\n");
    ierr = MatHeaderCopy(A,B);CHKERRQ(ierr);
  }

  //fprintf(stderr, "MatTranspose return\n");
  PetscFunctionReturn(0);
}
