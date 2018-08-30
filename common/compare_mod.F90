!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------

module compare_mod
  use constants_mod, only : i_def, r_def, l_def
  implicit none
contains

  function compare(data1, data2, ndata, print_me) result(count)
    implicit none
    integer(kind=i_def),                intent(in) :: ndata
    real(kind=r_def), dimension(ndata), intent(in) :: data1
    real(kind=r_def), dimension(ndata), intent(in) :: data2
    logical(kind=l_def),                intent(in) :: print_me

    integer(kind=i_def) :: count
    integer(kind=i_def) :: df
    real(kind=r_def) :: eps = 1.0e-014
    real(kind=r_def) :: diff

    count=0
    do df = 1, ndata
     diff = abs( ( data2(df) - data1(df) ) / ( data2(df) + data1(df) ) )
     if (diff > eps ) then
        count = count + 1
        if(print_me) then
           write(*,'(A,I6,3(":",E23.16))') "compare:", df, diff, data1(df), data2(df)
        end if
     end if
  end do
  end function compare
    
  
end module compare_mod
