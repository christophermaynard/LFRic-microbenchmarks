!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------

program cma_appinv_driver
  ! Based on the PSY-layer code generated from gw_pressure_precon_alg_mod.x90
  ! in the gravity_wave miniapp
  use constants_mod, only: r_def, i_def
  use columnwise_op_appinv_kernel_mod, only : columnwise_op_appinv_kernel_code
  use dino_mod, only : dino_type
  use compare_mod, only : compare
  implicit none

  ! mesh sizes
  integer(kind=i_def) :: ncell_2d, nlayers

  ! related to cma
  integer(kind=i_def) :: cma_bandwidth, cma_alpha, &
                         cma_beta, cma_gamma_m,    &
                         cma_gamma_p

  ! colouring sizes and arrays
  integer(kind=i_def)                              :: ncolours
  integer(kind=i_def), allocatable, dimension(:)   :: ncells_per_colour
  integer(kind=i_def), allocatable, dimension(:,:) :: cmap

  ! number of dofs (and unique dofs) in function space
  integer(kind=i_def) :: ndf, undf
  ! dof-maps for function space
  integer(kind=i_def), allocatable, dimension(:,:) :: map_any_space_1_y_vector

  integer(kind=i_def), allocatable, dimension(:) :: indirection_map

  ! the data
  real(kind=r_def), allocatable, dimension(:)     :: data1
  real(kind=r_def), allocatable, dimension(:)     :: data2
  real(kind=r_def), allocatable, dimension(:)     :: answer
  real(kind=r_def), allocatable, dimension(:,:,:) :: cma_matrix

  type(dino_type) :: dino

  ! loop counters
  integer(kind=i_def) :: colour, cell

  integer(kind=i_def) :: count

  ! make the reader
  dino = dino_type()

  ! ingest the data
  ! read scalars
  call dino%input_scalar(ncolours)
  call dino%input_scalar(ncell_2d)
  call dino%input_scalar(nlayers)
  call dino%input_scalar(cma_bandwidth)
  call dino%input_scalar(cma_alpha)
  call dino%input_scalar(cma_beta)
  call dino%input_scalar(cma_gamma_m)
  call dino%input_scalar(cma_gamma_p)
  call dino%input_scalar(ndf)
  call dino%input_scalar(undf)

  ! allocate and read arrays
  allocate(ncells_per_colour(ncolours))
  call dino%input_array(ncells_per_colour,ncolours)

  allocate(cmap(ncolours,maxval(ncells_per_colour)))
  allocate(data1(undf))
  allocate(data2(undf))
  allocate(cma_matrix(cma_bandwidth, nlayers, ncell_2d))
  allocate(map_any_space_1_y_vector(ndf, ncell_2d))
  allocate(indirection_map(nlayers))
  allocate(answer(undf))

  call dino%input_array(cmap,ncolours,maxval(ncells_per_colour))
  call dino%input_array(data1, undf)
  call dino%input_array(data2, undf)
  call dino%input_array(cma_matrix, cma_bandwidth, nlayers, ncell_2d)
  call dino%input_array(map_any_space_1_y_vector, ndf, ncell_2d)
  call dino%input_array(indirection_map, nlayers)
  call dino%input_array(answer, undf)

  write(6,'(A)') "cma_appinv_driver:ingested dinodump"
  !
  do colour=1, ncolours
  !$omp parallel default(shared), private(cell)
  !$omp do schedule(static)
     do cell=1,ncells_per_colour(colour)

       call columnwise_op_appinv_kernel_code(cmap(colour, cell), ncell_2d,    &
            data1, data2, cma_matrix, nlayers, cma_bandwidth, cma_alpha,      &
            cma_beta, cma_gamma_m, cma_gamma_p, ndf, undf,                    &
            map_any_space_1_y_vector(:,cmap(colour, cell)), indirection_map)
     end do
  !$omp end do
  !$omp end parallel
  end do

  write(6,'(A)') "cma_appinv_driver:Kernel run, checking answer ..."
  !check the answer
  count = compare(data1, answer, undf, .true. )
  write(6,'(A,I0,A,I0,A)') "cma_appinv_driver:checked ", undf, &
                           " answers, found ", count, " errors"

  ! deallocate the arrays
  deallocate(ncells_per_colour)
  deallocate(cmap)
  deallocate(data1)
  deallocate(data2)
  deallocate(cma_matrix)
  deallocate(map_any_space_1_y_vector)
  deallocate(indirection_map)
  deallocate(answer)

end program cma_appinv_driver
