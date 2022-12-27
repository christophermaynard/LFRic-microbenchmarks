!-----------------------------------------------------------------------------
! Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
! For further details please refer to the file LICENCE.original which you
! should have received as part of this distribution.
!-----------------------------------------------------------------------------

module compare_mod
  use constants_mod, only : i_def, r_def, l_def
  implicit none
contains

  function compare(data1, data2, ndata, print_me, tol) result(count)
    implicit none
    integer(kind=i_def),                intent(in) :: ndata
    real(kind=r_def), dimension(ndata), intent(in) :: data1
    real(kind=r_def), dimension(ndata), intent(in) :: data2
    logical(kind=l_def),                intent(in) :: print_me
    real(kind=r_def), optional,         intent(in) :: tol

    integer(kind=i_def) :: count
    integer(kind=i_def) :: df
    real(kind=r_def) :: abs_tol = 1.0e-14
    real(kind=r_def) :: diff, eps

    if(present(tol)) then
       eps=tol
    else
       eps=abs_tol
    end if

    count=0
    do df = 1, ndata
     diff = abs( ( data2(df) - data1(df) ) / ( data2(df) + data1(df) ) )
     if (diff > eps ) then
        count = count + 1
        if(print_me) then
           write(*,'(A,I6,3(":",E23.16),":",E8.3)') "compare:", df, diff, data1(df), data2(df),eps
        end if
     end if
  end do
  end function compare
    
  
end module compare_mod
