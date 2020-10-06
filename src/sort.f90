!*************************************************************************************************************************
!
!                                                    S O R T
!
!  Module:       SORT 
!
!  Programmer:   Pieter S. M. Smets
!                R&D depart. of Seismology and Acoustics - Koninklijk Nederlands Meteorologisch Instituut (KNMI)
!                De Bilt, The Netherlands
!
!  Date:         May 20, 2016
!
!  Language:     Fortran-90
!
!  Description:  This module includes several subroutines to sort data.
!
!
!*************************************************************************************************************************

Module sort

  use io_types

Contains

!
! =========================================================================================
!
!.......subroutine cocktailSortIx
!
!	Written by:  Pieter Smets, KNMI, 1 June 2012
!
!       Subroutine to sort integer*8 data using the cocktail
!       sort algorithm, returning an additional sort index
!       vector to sort other vectors or arrays the same way.
!       
!	INOUT:
!
!	A:     data vector
!
!       IN:
!
!       lim:   limit the sort program untill this index
!
!       OUT:
!
!       ix:    sorted index vector
!
!**************************** 
  Subroutine cocktailsortix8 ( A, lim, ix )
!****************************
! 
    implicit none
!
!.....Dummy variables.
!
    integer(int64), dimension(:), intent(inout)     :: A
    integer(int32), intent(in)                       :: lim
    integer(int32), dimension(size(A)), intent(out) :: ix
!
!.....Local variables.
!
    logical   :: swapped
    integer(int32) :: i,first,last,d
    integer(int64) :: dummy
!
! ----------
!
!.....Check
!
    if (lim.gt.size(a)) then
      print "(A)", 'ERROR: lim should be smaller or equal the length of A'
    endif
!
!.....Intialize.
!
    ix = (/ (i,i=1,size(A)) /)
    first=0
    swapped=.true.
!
    if (lim > 0) then
      last=lim-1
    else
      last=size(a)-1
    endif
!
!.....Do cocktailsort loop.
!
    do while (swapped)
!
      swapped=.false.
!
!........Increase start because all elements before are in correct order.
!
      first=first+1
!
!........Loop from start to end.
!
      do i=first,last,1
        if ( a(i)>a(i+1) ) then
          dummy=a(i)
          a(i)=a(i+1)
          a(i+1)=dummy
          d=ix(i)
          ix(i)=ix(i+1)
          ix(i+1)=d
          swapped=.true.
        end if
      end do
!
      if (swapped .eqv. .false.) exit
!
      swapped=.false.
!
!........Decrease last because elements after are in correct order.
!
      last=last-1
!
!........Loop from end to start.
!
      do i=last,first,-1
        if ( a(i)>a(i+1) ) then
          dummy=a(i)
          a(i)=a(i+1)
          a(i+1)=dummy
          d=ix(i)
          ix(i)=ix(i+1)
          ix(i+1)=d
          swapped=.true.
        end if
      end do
!
    end do
!          
    return
!
  End subroutine cocktailSortIx8
!
! =========================================================================================


! =========================================================================================
!
!.......subroutine cocktailSortI8
!
!	Written by:  Pieter Smets, KNMI, 1 June 2012
!
!       Subroutine to sort integer*8 data using the cocktail
!       sort algorithm.
!       
!	INOUT:
!
!	A:     data vector
!
!**************************** 
  Subroutine cocktailSortI8 ( A )
!****************************
! 
    implicit none
!
!.....Dummy variables.
!
    integer(int64), dimension(:), intent(inout) :: A
!
!.....Local variables.
!
    logical        :: swapped
    integer(int32) :: i,first,last
    integer(int64) :: dummy
!
! ----------
!
!.....Intialize.
!
    first=0
    last=size(a)-1
    swapped=.true.
!
!.....Do cocktailsort loop.
!
    do while (swapped)
!
      swapped=.false.
!
!........Increase start because all elements before are in correct order.
!
      first=first+1
!
!........Loop from start to end.
!
      do i=first,last,1
        if ( a(i)<a(i+1) ) then
          dummy=a(i)
          a(i)=a(i+1)
          a(i+1)=dummy
          swapped=.true.
        end if
      end do
!
      if (swapped .eqv. .false.) exit
!
      swapped=.false.
!
!........Decrease last because elements after are in correct order.
!
      last=last-1
!
!........Loop from end to start.
!
      do i=last,first,-1
        if ( a(i)<a(i+1) ) then
          dummy=a(i)
          a(i)=a(i+1)
          a(i+1)=dummy
          swapped=.true.
        end if
      end do
!
    end do
!          
    return
!
  End subroutine cocktailSortI8
!
! =========================================================================================


! =========================================================================================
!
!.......subroutine cocktailSort_I4
!
!	Written by:  Pieter Smets, KNMI, 1 June 2012
!
!       Subroutine to sort integer*8 data using the cocktail
!       sort algorithm.
!       
!	INOUT:
!
!	A:     data vector
!
!**************************** 
  Subroutine cocktailSort_I4 ( A )
!****************************
! 
    implicit none
!
!.....Dummy variables.
!
    integer(int32), dimension(:), intent(inout) :: A
!
!.....Local variables.
!
    logical        :: swapped
    integer(int32) :: i,first,last
    integer(int32) :: dummy
!
! ----------
!
!.....Intialize.
!
    first=0
    last=size(a)-1
    swapped=.true.
!
!.....Do cocktailsort loop.
!
    do while (swapped)
!
      swapped=.false.
!
!........Increase start because all elements before are in correct order.
!
      first=first+1
!
!........Loop from start to end.
!
      do i=first,last,1
        if ( a(i) .gt. a(i+1) ) then
          dummy=a(i)
          a(i)=a(i+1)
          a(i+1)=dummy
          swapped=.true.
        end if
      end do
!
      if (swapped .eqv. .false.) exit
!
      swapped=.false.
!
!........Decrease last because elements after are in correct order.
!
      last=last-1
!
!........Loop from end to start.
!
      do i=last,first,-1
        if ( a(i) .gt. a(i+1) ) then
          dummy=a(i)
          a(i)=a(i+1)
          a(i+1)=dummy
          swapped=.true.
        end if
      end do
!
    end do
!          
    return
!
  End subroutine cocktailSort_I4
!
! =========================================================================================


! =========================================================================================
!
!
!**************************** 
  Subroutine GET_STEPSIZE ( x, dx )
!****************************
! 
    implicit none
!
!.....Dummy variables.
!
    real(double), dimension(:), intent(in)  :: x
    real(double)              , intent(out)  :: dx
!
!.....Local variables.
!
    integer(int32) :: i
    real(double)   :: xmax1, xmax2
!
! ----------
!
!.....Scan for max, and element 1 step smaller
!
    xmax1 = 0._double
    xmax2 = 0._double
!
    do i = 1, size ( x, 1 )
      if ( x(i) .gt. xmax1 .and. x(i) .gt. xmax2 ) then
        xmax1 = x(i)
      else if ( x(i) .lt. xmax1 .and. x(i) .gt. xmax2 ) then
        xmax2 = x(i)
      end if
    end do
!
    dx = xmax1 - xmax2
!          
    return
!
  End subroutine get_stepsize
!
! =========================================================================================


! =========================================================================================
!
!.......subroutine cocktailSortIx4
!
!	Written by:  Pieter Smets, KNMI, 1 June 2012
!
!       Subroutine to sort integer*8 data using the cocktail
!       sort algorithm, returning an additional sort index
!       vector to sort other vectors or arrays the same way.
!       
!	INOUT:
!
!	A:     data vector
!
!       IN:
!
!       lim:   limit the sort program untill this index
!
!       OUT:
!
!       ix:    sorted index vector
!
!**************************** 
  Subroutine cocktailSortIx4 ( A, lim, ix )
!****************************
! 
    implicit none
!
!.....Dummy variables.
!
    integer(int32), dimension(:), intent(inout)     :: A
    integer(int32), intent(in)                       :: lim
    integer(int32), dimension(size(A)), intent(out) :: ix
!
!.....Local variables.
!
    logical   :: swapped
    integer(int32) :: i,first,last,d, dummy
!
! ----------
!
!.....Check
!
    if (lim > size(a)) then
      print "(a)", 'ERROR: lim should be smaller than the length of A'
    endif
!
!.....Intialize.
!
    ix = (/ (i,i=1,size(a)) /)
    first=0
    swapped=.true.
!
    if (lim > 0) then
      last=lim-1
    else
      last=size(a)-1
    endif
!
!.....Do cocktailsort loop.
!
    do while (swapped)
!
      swapped=.false.
!
!........Increase start because all elements before are in correct order.
!
      first=first+1
!
!........Loop from start to end.
!
      do i=first,last,1
        if ( a(i)>a(i+1) ) then
          dummy=a(i)
          a(i)=a(i+1)
          a(i+1)=dummy
          d=ix(i)
          ix(i)=ix(i+1)
          ix(i+1)=d
          swapped=.true.
        end if
      end do
!
      if (swapped .eqv. .false.) exit
!
      swapped=.false.
!
!........Decrease last because elements after are in correct order.
!
      last=last-1
!
!........Loop from end to start.
!
      do i=last,first,-1
        if ( a(i)>a(i+1) ) then
          dummy=a(i)
          a(i)=a(i+1)
          a(i+1)=dummy
          d=ix(i)
          ix(i)=ix(i+1)
          ix(i+1)=d
          swapped=.true.
        end if
      end do
!
    end do
!          
    return
!
  End subroutine cocktailSortIx4
!
! =========================================================================================

! =========================================================================================
!
!.......subroutine cocktailSort_DBLE
!
!	Written by:  Pieter Smets, KNMI, 1 June 2012
!
!       Subroutine to sort double precision data using the cocktail
!       sort algorithm.
!       
!	INOUT:
!
!	A:     data vector
!
!**************************** 
  Subroutine cocktailSort_DBLE ( A )
!****************************
! 
    implicit none
!
!.....Dummy variables.
!
    real(double), dimension(:), intent(inout) :: A
!
!.....Local variables.
!
    logical          :: swapped
    integer(int32)        :: i,first,last
    real(double) :: dummy
!
! ----------
!
!.....Intialize.
!
    first=0
    last=size(a)-1
    swapped=.true.
!
!.....Do cocktailsort loop.
!
    do while (swapped)
!
      swapped=.false.
!
!........Increase start because all elements before are in correct order.
!
      first=first+1
!
!........Loop from start to end.
!
      do i=first,last,1
        if ( a(i) .gt. a(i+1) ) then
          dummy=a(i)
          a(i)=a(i+1)
          a(i+1)=dummy
          swapped=.true.
        end if
      end do
!
      if (swapped .eqv. .false.) exit
!
      swapped=.false.
!
!........Decrease last because elements after are in correct order.
!
      last=last-1
!
!........Loop from end to start.
!
      do i=last,first,-1
        if ( a(i) .gt. a(i+1) ) then
          dummy=a(i)
          a(i)=a(i+1)
          a(i+1)=dummy
          swapped=.true.
        end if
      end do
!
    end do
!          
    return
!
  End subroutine cocktailSort_DBLE
!
! =========================================================================================



! =========================================================================================
!
!.......subroutine cocktailSortIx_DBLE
!
!	Written by:  Pieter Smets, KNMI, 1 June 2012
!
!       Subroutine to sort integer*8 data using the cocktail
!       sort algorithm, returning an additional sort index
!       vector to sort other vectors or arrays the same way.
!       
!	INOUT:
!
!	A:     data vector
!
!       IN:
!
!       lim:   limit the sort program untill this index
!
!       OUT:
!
!       ix:    sorted index vector
!
!**************************** 
  Subroutine cocktailSortIx_DBLE ( B, lim, ix )
!****************************
! 
    implicit none
!
!.....Dummy variables.
!
    real(double)  , dimension(:)      , intent(in)   :: B
    integer(int32)                    , intent(in)   :: lim
    integer(int32), dimension(size(B)), intent(out)  :: ix
!
!.....Local variables.
!
    logical          :: swapped
    integer(int32)   :: i, first, last, d
    real(double)     :: dummy
    real(double), dimension(size(B)) :: A 
!
! ----------
!
!.....Check
!
    IF ( lim .GT. size(A) ) THEN
      PRINT "(A)", 'ERROR: lim should be smaller than the length of A'
    END IF
!
!.....Intialize.
!
    a       = b
    first   = 0
    ix      = (/ ( i, i = 1, size(b) ) /)
    swapped = .true.
!
    if ( lim .gt. 0 ) then
      last = lim - 1
    else
      last = size(b) - 1
    end if
!
!.....Do cocktailsort loop.
!
    do while ( swapped )
!
      swapped = .false.
!
!........Increase start because all elements before are in correct order.
!
      first = first + 1
!
!........Loop from start to end.
!
      do i = first, last, 1
        if ( a( i ) .gt. a( i + 1 ) ) then
          dummy       = a( i )
          a( i )      = a( i + 1 )
          a( i + 1 )  = dummy
          d           = ix(i)
          ix(i)     = ix( i + 1 )
          ix( i + 1 ) = d
          swapped = .true.
        end if
      end do
!
      if ( .not. swapped ) exit
!
      swapped = .false.
!
!........Decrease last because elements after are in correct order.
!
      last = last - 1
!
!........Loop from end to start.
!
      do i = last, first, - 1
        if ( a( i ) .gt. a( i + 1 ) ) then
          dummy       = a( i )
          a( i )      = a( i + 1 )
          a( i + 1 )  = dummy
          d           = ix(i)
          ix(i)     = ix( i + 1 )
          ix( i + 1 ) = d
          swapped = .true.
        end if
      end do
!
    end do
!          
    return
!
  End subroutine cocktailSortIx_DBLE
!
! =========================================================================================


! =========================================================================================
!
!.......subroutine cocktailSortA
!
!	Written by:  Pieter Smets, KNMI, 1 June 2012
!
!       Subroutine to sort character and integer data using the
!       cocktail sort algorithm.
!       
!	INOUT:
!
!	A:     data vector
!
!**************************** 
  Subroutine cocktailSortA ( chars, index )
!****************************
! 
    implicit none
!
!.....Dummy variables.
!
    character(len=*), dimension(:), intent(inout) :: chars
    integer(int32)  , dimension(size(chars,1)), intent(out), optional :: index
!
!.....Local variables.
!
    logical   :: swapped
    integer(int32) :: i,j,nofstrs,nofchar,first,last,d
    integer(int32), dimension(len(chars))  :: dummy
    integer(int32), dimension(size(chars)) :: ix
    integer(int32), dimension(128)         :: a2i
    integer(int32), dimension(len(chars(1)),size(chars)) :: A
    character(len=len(chars(1)))      :: str
!
    DATA a2i /-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
          & -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
          & -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0, &
          & 1,2,3,4,5,6,7,8,9,-1,-1,-1,-1,-1,-1,-1, &
          & 10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40, &
          & 42,44,46,48,50,52,54,56,58,60,-1,-1,-1,-1,-1,-1, &
          & 11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41, &
          & 43,45,47,49,51,53,55,57,59,61,-1,-1,-1,-1,-1,-1/
!
! ----------
!
!.....Chars to integer
!
    nofstrs=size(chars)
    nofchar=len(chars(1))
    do i=1,nofstrs
      ix(i)=i
      str=chars(i)
      do j=1,nofchar
        a(j,i)=a2i(ichar(str(j:j)))
      enddo
    enddo
!
!.....Intialize.
!
    swapped=.true.
    first=0
    last=nofstrs-1
!
!.....Do cocktailsort loop.
!
    do while (swapped)
!
      swapped=.false.
!
!........Increase start because all elements before are in correct order.
!
      first=first+1
!
!........Loop from start to end.
!
      do i=first,last,1
        do j=1,nofchar
          if ( a(j,i)>a(j,i+1) ) then
            dummy=a(:,i)
            a(:,i)=a(:,i+1)
            a(:,i+1)=dummy
            d=ix(i)
            ix(i)=ix(i+1)
            ix(i+1)=d
            swapped=.true.
            exit
          elseif (a(j,i)<a(j,i+1)) then
            exit
          end if
        enddo
      enddo
!
      if (swapped.eqv..false.) exit
!
      swapped=.false.
!
!........Decrease last because elements after are in correct order.
!
      last=last-1
!
!........Loop from end to start.
!
      do i=last,first,-1
        do j=1,nofchar
          if ( a(j,i)>a(j,i+1) ) then
            dummy=a(:,i)
            a(:,i)=a(:,i+1)
            a(:,i+1)=dummy
            d=ix(i)
            ix(i)=ix(i+1)
            ix(i+1)=d
            swapped=.true.
            exit
          elseif (a(j,i)<a(j,i+1)) then
            exit
          end if
        enddo
      enddo
!
    enddo
!
!.....apply sort to character vector
!
    chars=chars(ix)
    if ( present(index) ) index = ix
!
    return
!
  End subroutine cocktailSortA
!
! =========================================================================================


! =========================================================================================
!
! Subroutine uniqueGrowingList_I4
!
!	Written by:  Pieter Smets, KNMI, 3 December 2012
!
!       Subroutine to evaluate if an item can be added to a unique list
!       
!	INOUT:
!
!	list:          data vector
!       cnt :          list length
!
!       IN:
!
!       item:          value to evaluate
!
!**************************** 
  Subroutine UniqueGrowingList_I4 ( item, list, cnt )
!****************************
! 
    implicit none
!
!.....Dummy variables.
!
    integer(int32), intent(in)                    :: item
    integer(int32), intent(inout)                 :: cnt
    integer(int32), dimension(:), intent(inout) :: list
!
!.....Local variables.
!
    integer(int32) :: i
!
! ----------
!
!.....Intialize.
!
    if (size(list).eq.cnt) then
      print "(A)", 'ERROR: UniqueGrowingList has reached maximum vector size. Initialize a larger list.'
      stop
    endif
!
!.....Check if item is member of list
!
    do i=1_int32,cnt
      if (item.eq.list(i)) return
    end do
!
!.....Add item to list
!
    cnt=cnt+1_int32
    list(cnt)=item
!          
    return
!
  End subroutine uniquegrowinglist_i4
!
! =========================================================================================


! =========================================================================================
!
!.......subroutine uniqueGrowingList_DBLE
!
!	Written by:  Pieter Smets, KNMI, 3 December 2012
!
!       Subroutine to evaluate if an item can be added to a unique list
!       
!	INOUT:
!
!	list:          data vector
!       cnt :          list length
!
!       IN:
!
!       item:          value to evaluate
!
!**************************** 
  Subroutine UniqueGrowingList_DBLE ( item, list, cnt )
!****************************
! 
    implicit none
!
!.....Dummy variables.
!
    real(double), intent(in)                  :: item
    integer(int32), intent(inout)             :: cnt
    real(double), dimension(:), intent(inout) :: list
!
!.....Local variables.
!
    integer(int32) :: i
!
! ----------
!
!.....Intialize.
!
    if (size(list) .eq. cnt) then
      print "(A)", 'ERROR: UniqueGrowingList has reached maximum vector size. Initialize a larger list.'
      stop
    endif
!
!.....Check if item is member of list
!
    do i=1_int32,cnt
      if (abs(item-list(i)).lt.1.d-10) return
    end do
!
!.....Add item to list
!
    cnt=cnt+1_int32
    list(cnt)=item
!          
    return
!
  End subroutine uniquegrowinglist_dble
!
! =========================================================================================


! =========================================================================================
!
!.......subroutine GetListItemIndex_I4
!
!	Written by:  Pieter Smets, KNMI, 3 December 2012
!
!       Subroutine to get the index of an item in a list
!       
!	IN:
!
!	list:          data vector
!       item:          value to evaluate
!
!       OUT:
!
!       ix:            index of item in list
!
!**************************** 
  Subroutine GetListItemIndex_I4 ( ix, item, list )
!****************************
! 
    implicit none
!
!.....Dummy variables.
!
    integer(int32), intent(in)                 :: item
    integer(int32), intent(out)                :: ix
    integer(int32), dimension(:), intent(in) :: list
!
! ----------
!
!.....loop over list
!
    do ix=1,size(list)
      if (item.eq.list(ix)) return
    end do
!
    print "(A)", 'ERROR: item not found in list! Program terminated.'
    stop
!
  End subroutine GetListItemIndex_I4
!
! =========================================================================================


! =========================================================================================
!
!.......subroutine GetListItemIndex_DBLE
!
!	Written by:  Pieter Smets, KNMI, 3 December 2012
!
!       Subroutine to get the index of an item in a list
!       
!	IN:
!
!	list:          data vector
!       item:          value to evaluate
!
!       OUT:
!
!       ix:            index of item in list
!
!**************************** 
  Subroutine GetListItemIndex_DBLE ( ix, item, list )
!****************************
! 
    implicit none
!
!.....Dummy variables.
!
    real(double)              , intent(in)   :: item
    integer(int32)            , intent(out)  :: ix
    real(double), dimension(:), intent(in)   :: list
!
! ----------
!
!.....loop over list
!
    do ix=1,size(list)
      if (abs(item-list(ix)).lt.1.d-10) return
    end do
!
    print "(A)", 'ERROR: item not found in list! Program terminated.'
    stop
!
  End subroutine GetListItemIndex_DBLE
!
! =========================================================================================
!
End module sort
