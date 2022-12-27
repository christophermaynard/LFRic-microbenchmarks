program helmholtz_driver

  USE constants_mod, ONLY: r_def, i_def
  USE apply_helmholtz_operator_kernel_mod, ONLY: apply_helmholtz_operator_code
  use dino_mod, only: dino_type
  use compare_mod, only : compare
  
  IMPLICIT NONE  
      
  real(kind=r_def), allocatable, dimension(:) :: y_vec, x_vec, ans

  real(kind=r_def), allocatable, dimension(:) :: helmholtz_operator1
  real(kind=r_def), allocatable, dimension(:) :: helmholtz_operator2
  real(kind=r_def), allocatable, dimension(:) :: helmholtz_operator3
  real(kind=r_def), allocatable, dimension(:) :: helmholtz_operator4
  real(kind=r_def), allocatable, dimension(:) :: helmholtz_operator5
  real(kind=r_def), allocatable, dimension(:) :: helmholtz_operator6
  real(kind=r_def), allocatable, dimension(:) :: helmholtz_operator7
  real(kind=r_def), allocatable, dimension(:) :: helmholtz_operator8
  real(kind=r_def), allocatable, dimension(:) :: helmholtz_operator9
  
  INTEGER(KIND=i_def) cell
  INTEGER(KIND=i_def) loop0_start, loop0_stop
  INTEGER(KIND=i_def) nlayers

  integer(kind=i_def) :: dummy, count
  
  INTEGER(KIND=i_def), allocatable, dimension(:,:) :: map_w3
  INTEGER(KIND=i_def) :: ndf_w3, undf_w3
  INTEGER(KIND=i_def) :: x_vec_max_branch_length
  INTEGER(KIND=i_def), allocatable, dimension(:,:) :: x_vec_stencil_size
  INTEGER(KIND=i_def), allocatable, dimension(:,:,:,:) :: x_vec_stencil_dofmap
  type(dino_type) :: steggy
  !
  steggy = dino_type()
      !
  call steggy%input_scalar(loop0_start)
  call steggy%input_scalar(loop0_stop)
  call steggy%input_scalar(nlayers)
  call steggy%input_scalar(undf_w3)
  call steggy%input_scalar(x_vec_max_branch_length)
  ndf_w3=1
  allocate(x_vec_stencil_size(4,undf_w3))
  allocate(x_vec_stencil_dofmap(ndf_w3, x_vec_max_branch_length,4,undf_w3))
  allocate(map_w3(ndf_w3,undf_w3))
  
  call steggy%input_array(x_vec_stencil_size, 4, undf_w3)
  call steggy%input_array(x_vec_stencil_dofmap,&
       ndf_w3,x_vec_max_branch_length, 4, undf_w3)
  call steggy%input_array(map_w3,ndf_w3,undf_w3)

  allocate(y_vec(undf_w3))
  allocate(x_vec(undf_w3))
  allocate(ans(undf_w3))
  
  call steggy%input_array(y_vec,undf_w3)
  call steggy%input_array(x_vec,undf_w3)
  
  allocate(helmholtz_operator1(undf_w3))
  allocate(helmholtz_operator2(undf_w3))
  allocate(helmholtz_operator3(undf_w3))
  allocate(helmholtz_operator4(undf_w3))
  allocate(helmholtz_operator5(undf_w3))
  allocate(helmholtz_operator6(undf_w3))
  allocate(helmholtz_operator7(undf_w3))
  allocate(helmholtz_operator8(undf_w3))
  allocate(helmholtz_operator9(undf_w3))
  
  call steggy%input_array(helmholtz_operator1,undf_w3)
  call steggy%input_array(helmholtz_operator2,undf_w3)
  call steggy%input_array(helmholtz_operator3,undf_w3)
  call steggy%input_array(helmholtz_operator4,undf_w3)
  call steggy%input_array(helmholtz_operator5,undf_w3)
  call steggy%input_array(helmholtz_operator6,undf_w3)
  call steggy%input_array(helmholtz_operator7,undf_w3)
  call steggy%input_array(helmholtz_operator8,undf_w3)
  call steggy%input_array(helmholtz_operator9,undf_w3)

  call steggy%input_array(ans,undf_w3)     
  write(*,*) "helmholtz_driver:ingested dinodump"  
  call steggy%dino_destructor()

  do dummy = 1, 1000
  
!  !$omp parallel default(shared), private(cell)
!  !$omp do schedule(static)
  DO cell=loop0_start,loop0_stop
     
     CALL apply_helmholtz_operator_code(nlayers, y_vec, x_vec, x_vec_stencil_size(:,cell), &
          &x_vec_max_branch_length, x_vec_stencil_dofmap(:,:,:,cell), helmholtz_operator1, helmholtz_operator2, &
          &helmholtz_operator3, helmholtz_operator4, helmholtz_operator5, &
          &helmholtz_operator6, helmholtz_operator7, helmholtz_operator8, &
          &helmholtz_operator9, .false., ndf_w3, undf_w3, map_w3(:,cell))
     
  END DO
!  !$omp end do
!  !$omp end parallel
end do

  write(*,'(A)') "helmholtz_driver:Kernel run, checking answer ..."
  !check the answer
  count = compare(y_vec, ans, undf_w3, .false.)
  write(*,'(A,I6,A,I6,A)') "helmholtz_driver:checked ",undf_w3," answers, found ",count, " errors" 
  
END program helmholtz_driver
