!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------

program lma_driver
  use constants_mod, only: r_def, i_def
  use matrix_vector_kernel_mod, only: matrix_vector_code
  use dino_mod, only : dino_type
  use compare_mod, only : compare
  implicit none
  
  ! mesh sizes
  integer(kind=i_def) :: ncell, ncell_3d, nlayers
  
  ! colouring sizes and arrays
  integer(kind=i_def)                              :: ncolours
  integer(kind=i_def), allocatable, dimension(:)   :: ncells_per_colour
  integer(kind=i_def), allocatable, dimension(:,:) :: cmap
  
  ! dof-maps for space 1
  integer(kind=i_def) :: ndf1, undf1
  integer(kind=i_def), allocatable, dimension(:,:) :: map1
  
  ! dof-maps for space 2
  integer(kind=i_def) :: ndf2, undf2
  integer(kind=i_def), allocatable, dimension(:,:) :: map2
  
  ! the data
  real(kind=r_def), allocatable, dimension(:)     :: data1
  real(kind=r_def), allocatable, dimension(:)     :: data2
  real(kind=r_def), allocatable, dimension(:)     :: answer
  real(kind=r_def), allocatable, dimension(:,:,:) :: op_data    

  type(dino_type) :: dino

  ! loop counters
  integer(kind=i_def) :: colour, cell

  integer(kind=i_def) :: count
  
  ! make the reader
  dino = dino_type()    

  !ingest the data
  call dino%input_scalar(ncell)
  call dino%input_scalar(ncell_3d)
  call dino%input_scalar(ncolours)
  call dino%input_scalar(nlayers)

  ! allocate the colour arrays
  allocate(ncells_per_colour(ncolours))
  call dino%input_array(ncells_per_colour,ncolours)
  allocate( cmap(ncolours,maxval(ncells_per_colour)) )
  call dino%input_array(cmap,ncolours,maxval(ncells_per_colour))

  ! read space sizes and allocate arrays
  call dino%input_scalar(ndf1)
  call dino%input_scalar(undf1)
  allocate(map1(ndf1,ncell))
  call dino%input_array(map1,ndf1, ncell)
  
  call dino%input_scalar(ndf2)
  call dino%input_scalar(undf2)
  allocate(map2(ndf2,ncell))
  call dino%input_array(map2,ndf2, ncell)

  ! allocate the floating point data arrays
  allocate( data1(undf1), data2(undf2) )
  allocate( op_data(ndf1,ndf2,ncell_3d) )
  allocate( answer(undf1) )
  
  ! read the floating point data
  call dino%input_array(op_data, ndf1, ndf2, ncell_3d)
  call dino%input_array(data1, undf1)
  call dino%input_array(data2, undf2)
  call dino%input_array(answer, undf1)
  
  write(*,'(A)') "lma_driver:ingested dinodump"
  !
  do colour=1, ncolours
  !$omp parallel default(shared), private(cell)
  !$omp do schedule(static)
     do cell=1,ncells_per_colour(colour)

        call matrix_vector_code(cmap(colour, cell), nlayers, data1, data2, &
             ncell_3d, op_data, ndf1, undf1, map1(:,cmap(colour,cell)), &
             ndf2, undf2, map2(:,cmap(colour,cell)) )

     end do
  !$omp end do
  !$omp end parallel
  end do

  write(*,'(A)') "lma_driver:Kernel run, checking answer ..."
  !check the answer
  count = compare(data1, answer, undf1, .true. )
  write(*,'(A,I6,A,I6,A)') "lma_driver:checked ",undf1," answers, found ",count, " errors" 
  

  ! deallocate the arrays
  deallocate(ncells_per_colour, cmap)
  deallocate(map1, map2)
  deallocate(data1, data2)
  deallocate(op_data, answer)
  
end program lma_driver
