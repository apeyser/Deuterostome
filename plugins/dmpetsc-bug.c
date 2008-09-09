#include <src/mat/impls/dense/mpi/mpidense.h>

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
