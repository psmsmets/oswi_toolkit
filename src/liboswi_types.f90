!*****************************************************************************80
!
!                                   L I B O S W I
!
!  Module:       LIBOSWI (include)
!
!  Programmer:   Pieter S. M. Smets
!                R&DSA depart. of Seismology and Acoustics
!                Koninklijk Nederlands Meteorologisch Instituut (KNMI)
!                De Bilt, The Netherlands
!
!  Date:         May 20, 2016
!
!  Language:     Fortran-90
!
!  Description:  liboswi types functions and subroutines include
!
!
!*****************************************************************************80

Subroutine oswi_clear ( oswi, dealloc )
!*****************************************************************************80
!
!! OSWI_CLEAR 
!
!  Description:
!
!    Clear and reset the entire oswi type structure. Optionally, array deallocation
!    can be disabled.  
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi ) oswi.
!
!    Optional input, logical DEALLOC, flag to toggle deallocating the arrays.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi), intent(inout)  :: oswi
  logical, intent(in), optional   :: dealloc
!
! clear oswi hdr and dat input types
!
  call oswi_clear_hdr( oswi%hdr, dealloc )
  call liboswi_clear_type_dat( oswi%dat, dealloc )
!
  return
!
!*****************************************************************************80
End subroutine oswi_clear


Subroutine oswi_clear_hdr ( hdr, dealloc )
!*****************************************************************************80
!
!! OSWI_CLEAR_HDR
!
!  Description:
!
!    Clear and reset the oswi hdr type structure. Optionally, array deallocation
!    can be disabled.  
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi ) oswi.
!
!    Optional input, logical DEALLOC, flag to toggle deallocating the arrays.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_hdr), intent(inout)  :: hdr
  logical, intent(in), optional       :: dealloc
!
! Local variables
!
  logical  :: do_deallocate
!
  do_deallocate=.true.
  if (present(dealloc)) do_deallocate=dealloc
!
! clear oswi input types
!
  call oswi_clear_type_env( hdr%env )
  call oswi_clear_type_osw( hdr%osw )
  call oswi_clear_type_dem( hdr%dem )
  call oswi_clear_type_grid( hdr%grid )
  call oswi_clear_type_f( hdr%f )
!
! out
!
  hdr%type=''
  hdr%history = ''
  hdr%name = ''
  hdr%long_name = ''
  hdr%standard_name = ''
  hdr%units = ''
  hdr%integrate=.false.
  hdr%fanalysis=.false.
  hdr%variance=.false.
  hdr%normalize=.false.
  hdr%Pa=.false.
  hdr%dB=.false.
  hdr%lg=.false.
!
  hdr%air=.false.
  hdr%sea=.false.
  hdr%bed=.false.
  hdr%swi=.false.
  hdr%swh=.false.
  hdr%sfd=.false.
!
! general
!
  hdr%id=0_int32
  hdr%referencePressure=int(1e6,int32) ! µPa -> 1Pa
  hdr%clip=.false.
  hdr%global=.false.
  hdr%debug=.false.
  hdr%missingValue=missing_int32
!
  return
!
!*****************************************************************************80
End subroutine oswi_clear_hdr


Subroutine liboswi_allocate_type_dat ( hdr, dat, fill, both )
!*****************************************************************************80
!
!! LIBOSWI_ALLOCATE_TYPE_DAT
!
!  Description:
!
!    Clear and reset the oswi dat type structure. Optionally, array deallocation
!    can be disabled.  
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi_dat ) dat.
!
!    Optional input, logical DEALLOC, flag to toggle deallocating the arrays.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_hdr), intent(in), target   :: hdr
  type(type_oswi_dat), intent(inout)        :: dat
  logical, intent(in), optional             :: fill, both
!
! Local variables
!
   logical :: allocBoth
!
   allocBoth=.false.
   if (present(both)) allocBoth=both
!
  if (hdr%integrate.or.allocBoth) call liboswi_allocate_type_dat_ll( hdr, dat%ll, fill )
  if (.not.hdr%integrate.or.allocBoth) call liboswi_allocate_type_dat_llf( hdr, dat%llf, fill )
!
  return
!
!*****************************************************************************80
End subroutine liboswi_allocate_type_dat


Subroutine liboswi_allocate_type_dat_ll ( hdr, dat_ll, fill )
!*****************************************************************************80
!
!! LIBOSWI_ALLOCATE_TYPE_DAT_LL
!
!  Description:
!
!    Allocate the oswi dat_ll type structure. Optionally, array initialization
!    can be disabled.  
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi_dat_ll ) dat_ll.
!
!    Optional input, logical fill, flag to toggle filling the variables.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_hdr), intent(in), target   :: hdr
  type(type_oswi_dat_ll), intent(inout)     :: dat_ll
  logical, intent(in), optional             :: fill
!
! Local variables
!
  logical  :: dofill
  integer(int32), pointer :: n
!
  dofill=.true.
  if (present(fill)) dofill=fill
!
  n=>hdr%grid%numberOfPoints
!
! allocate
!
! dat
  if (allocated(dat_ll%dat).and.size(dat_ll%dat,1).ne.n) deallocate(dat_ll%dat)
  if (.not.allocated(dat_ll%dat)) allocate(dat_ll%dat(n))
! mask
  if (allocated(dat_ll%mask).and.size(dat_ll%mask,1).ne.n) deallocate(dat_ll%mask)
  if (.not.allocated(dat_ll%mask)) allocate(dat_ll%mask(n))
!
  if (hdr%fanalysis) then
!   f_dom
    if (allocated(dat_ll%fd).and.size(dat_ll%fd,1).ne.n) deallocate(dat_ll%fd)
    if (.not.allocated(dat_ll%fd)) allocate(dat_ll%fd(n))
!   f_cen
    if (allocated(dat_ll%fc).and.size(dat_ll%fc,1).ne.n) deallocate(dat_ll%fc)
    if (.not.allocated(dat_ll%fc)) allocate(dat_ll%fc(n))
!   f_bw
    if (allocated(dat_ll%fb).and.size(dat_ll%fb,1).ne.n) deallocate(dat_ll%fb)
    if (.not.allocated(dat_ll%fb)) allocate(dat_ll%fb(n))
!   f_rms
    if (allocated(dat_ll%fr).and.size(dat_ll%fr,1).ne.n) deallocate(dat_ll%fr)
    if (.not.allocated(dat_ll%fr)) allocate(dat_ll%fr(n))
!
  end if
!
  if (dofill) call liboswi_clear_type_dat_ll(dat_ll=dat_ll,dealloc=.false.)
!
  nullify(n)
!
  return
!
!*****************************************************************************80
End subroutine liboswi_allocate_type_dat_ll


Subroutine liboswi_allocate_type_dat_llf ( hdr, dat_llf, fill )
!*****************************************************************************80
!
!! LIBOSWI_ALLOCATE_TYPE_DAT_LLF
!
!  Description:
!
!    Allocate the oswi dat_llf type structure. Optionally, array initialization
!    can be disabled.  
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi_dat_llf ) dat_llf.
!
!    Optional input, logical DEALLOC, flag to toggle deallocating the arrays.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_hdr)    , intent(in), target   :: hdr
  type(type_oswi_dat_llf), intent(inout)        :: dat_llf
  logical, intent(in), optional                 :: fill
!
! Local variables
!
  logical  :: dofill
  integer(int32), pointer :: n, f
!
  dofill=.true.
  if (present(fill)) dofill=fill
!
  n=>hdr%grid%numberOfPoints
  f=>hdr%f%size
!
! allocate
!
! dat
  if (allocated(dat_llf%dat).and.(size(dat_llf%dat,1).ne.n.or.size(dat_llf%dat,2).ne.f)) &
    deallocate(dat_llf%dat)
  if (.not.allocated(dat_llf%dat)) allocate(dat_llf%dat(n,f))
! mask
  if (allocated(dat_llf%mask).and.(size(dat_llf%mask,1).ne.n.or.size(dat_llf%mask,2).ne.f)) &
    deallocate(dat_llf%mask)
  if (.not.allocated(dat_llf%mask)) allocate(dat_llf%mask(n,f))
!
  if (dofill) call liboswi_clear_type_dat_llf(dat_llf=dat_llf,dealloc=.false.)
!
  nullify(n,f)
!
  return
!
!*****************************************************************************80
End subroutine liboswi_allocate_type_dat_llf


Subroutine liboswi_clear_type_dat ( dat, dealloc )
!*****************************************************************************80
!
!! LIBOSWI_CLEAR_TYPE_DAT
!
!  Description:
!
!    Clear and reset the oswi dat type structure. Optionally, array deallocation
!    can be disabled.  
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi_dat ) dat.
!
!    Optional input, logical DEALLOC, flag to toggle deallocating the arrays.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_dat), intent(inout)  :: dat
  logical, intent(in), optional       :: dealloc
!
  dat%epoch=0_int64
  dat%time=''
  dat%file=''
!
  call liboswi_clear_type_dat_ll(dat%ll,dealloc)
  call liboswi_clear_type_dat_llf(dat%llf,dealloc)
!
  return
!
!*****************************************************************************80
End subroutine liboswi_clear_type_dat


Subroutine liboswi_clear_type_dat_llf ( dat_llf, dealloc )
!*****************************************************************************80
!
!! LIBOSWI_CLEAR_TYPE_DAT_LL
!
!  Description:
!
!    Clear and reset the oswi dat_llf type structure. Optionally, array deallocation
!    can be disabled.  
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi_dat_llf ) dat_llf.
!
!    Optional input, logical DEALLOC, flag to toggle deallocating the arrays.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_dat_llf), intent(inout) :: dat_llf
  logical, intent(in), optional          :: dealloc
!
! Local variables
!
  logical  :: do_deallocate
!
  do_deallocate=.true.
  if (present(dealloc)) do_deallocate=dealloc
!
  dat_llf%maxValue=0._double
!
  if (do_deallocate) then
    if (allocated(dat_llf%dat)) deallocate(dat_llf%dat)
    if (allocated(dat_llf%mask)) deallocate(dat_llf%mask)
  else
    if (allocated(dat_llf%dat)) dat_llf%dat=0._double
    if (allocated(dat_llf%mask)) dat_llf%mask=.false.
  end if
!
  return
!
!*****************************************************************************80
End subroutine liboswi_clear_type_dat_llf


Subroutine liboswi_clear_type_dat_ll ( dat_ll, dealloc )
!*****************************************************************************80
!
!! LIBOSWI_CLEAR_TYPE_DAT_LL
!
!  Description:
!
!    Clear and reset the oswi dat_ll type structure. Optionally, array deallocation
!    can be disabled.  
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi_dat_ll ) dat_ll.
!
!    Optional input, logical DEALLOC, flag to toggle deallocating the arrays.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_dat_ll), intent(inout)  :: dat_ll
  logical, intent(in), optional          :: dealloc
!
! Local variables
!
  logical  :: do_deallocate
!
  do_deallocate=.true.
  if (present(dealloc)) do_deallocate=dealloc
!
  dat_ll%maxValue=0._double
!
  if (do_deallocate) then
    if (allocated(dat_ll%dat))  deallocate(dat_ll%dat)
    if (allocated(dat_ll%mask)) deallocate(dat_ll%mask)
    if (allocated(dat_ll%fc))   deallocate(dat_ll%fc)
    if (allocated(dat_ll%fd))   deallocate(dat_ll%fd)
    if (allocated(dat_ll%fb))   deallocate(dat_ll%fb)
    if (allocated(dat_ll%fr))   deallocate(dat_ll%fr)
  else
    if (allocated(dat_ll%dat))  dat_ll%dat=0._double
    if (allocated(dat_ll%mask)) dat_ll%mask=.false.
    if (allocated(dat_ll%fd))   dat_ll%fd=0._double
    if (allocated(dat_ll%fc))   dat_ll%fc=0._double
    if (allocated(dat_ll%fb))   dat_ll%fb=0._double
    if (allocated(dat_ll%fr))   dat_ll%fr=0._double
  end if
!
  return
!
!*****************************************************************************80
End subroutine liboswi_clear_type_dat_ll


Subroutine oswi_stack_clear ( oswi_stack, dealloc )
!*****************************************************************************80
!
!! OSWI_CLEAR 
!
!  Description:
!
!    Clear and reset the entire oswi type structure. Optionally, array deallocation
!    can be disabled.  
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi ) oswi.
!
!    Optional input, logical DEALLOC, flag to toggle deallocating the arrays.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_stack), intent(inout)  :: oswi_stack
  logical, intent(in), optional   :: dealloc
!
! clear oswi hdr and dat input types
!
  call oswi_clear_hdr( oswi_stack%hdr, dealloc )
  call oswi_stack_clear_dat( oswi_stack%dat, dealloc )
!
  return
!
!*****************************************************************************80
End subroutine oswi_stack_clear


Subroutine oswi_stack_clear_dat_var ( var, dealloc )
!*****************************************************************************80
!
!! oswi_stack_clear_dat_var
!
!  Description:
!
!    Clear and reset the oswi dat type structure. Optionally, array deallocation
!    can be disabled.  
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi_dat ) dat.
!
!    Optional input, logical DEALLOC, flag to toggle deallocating the arrays.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_stack_dat_var), intent(inout)  :: var
  logical, intent(in), optional             :: dealloc
!
! Local variables
!
  logical  :: do_deallocate
!
  do_deallocate=.true.
  if (present(dealloc)) do_deallocate=dealloc
!
  if (do_deallocate) then
    if (allocated(var%ll)) deallocate(var%ll)
    if (allocated(var%fc)) deallocate(var%fc)
    if (allocated(var%fd)) deallocate(var%fd)
    if (allocated(var%fb)) deallocate(var%fb)
    if (allocated(var%fr)) deallocate(var%fr)
    if (allocated(var%llf)) deallocate(var%llf)
  else
    var%ll=0._double
    var%fc=0._double
    var%fd=0._double
    var%fb=0._double
    var%fr=0._double
    var%llf=0._double
  end if
!
  return
!
!*****************************************************************************80
End subroutine oswi_stack_clear_dat_var


Subroutine oswi_stack_clear_dat ( dat, dealloc )
!*****************************************************************************80
!
!! OSWI_STACK_CLEAR_DAT
!
!  Description:
!
!    Clear and reset the oswi dat type structure. Optionally, array deallocation
!    can be disabled.  
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi_dat ) dat.
!
!    Optional input, logical DEALLOC, flag to toggle deallocating the arrays.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_stack_dat), intent(inout)  :: dat
  logical, intent(in), optional             :: dealloc
!
! Local variables
!
  logical  :: do_deallocate
!
  do_deallocate=.true.
  if (present(dealloc)) do_deallocate=dealloc
!
  dat%numberOfFiles=0_int32  
  dat%maxValue=0._double
  dat%significance_level=0._double
!
  call oswi_stack_clear_dat_var(var=dat%mu,dealloc=do_deallocate)
  call oswi_stack_clear_dat_var(var=dat%mu,dealloc=do_deallocate)
  call oswi_stack_clear_dat_var(var=dat%mu,dealloc=do_deallocate)
  call oswi_stack_clear_dat_var(var=dat%mu,dealloc=do_deallocate)
  call oswi_stack_clear_dat_var(var=dat%mu,dealloc=do_deallocate)
!
  if (do_deallocate) then
    if (allocated(dat%epoch)) deallocate(dat%epoch)
    if (allocated(dat%mask)) deallocate(dat%mask)
    if (allocated(dat%w)) deallocate(dat%w)
    if (allocated(dat%wf)) deallocate(dat%wf)
    if (associated(dat%f)) nullify(dat%f)
  else
    dat%epoch=0_int64
    dat%w=0._double
    dat%wf=0._double
    dat%mask=.false.
  end if
!
  return
!
!*****************************************************************************80
End subroutine oswi_stack_clear_dat


Subroutine oswi_list_clear ( oswi_list, dealloc )
!*****************************************************************************80
!
!! OSWI_LIST_CLEAR 
!
!  Description:
!
!    Clear and reset the entire oswi type structure. Optionally, array deallocation
!    can be disabled.  
!
!  Modified:
!
!    12 Juli 2017
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi_list ) oswi_list.
!
!    Optional input, logical DEALLOC, flag to toggle deallocating the arrays.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_list), intent(inout)  :: oswi_list
  logical, intent(in), optional   :: dealloc
!
! Local variables
!
  logical        :: do_deallocate
  integer(int32) :: i
!
! clear oswi_list hdr and dat input types
!
  call oswi_clear_hdr( oswi_list%hdr, dealloc )
!
  do_deallocate=.true.
  if (present(dealloc)) do_deallocate=dealloc
!
  oswi_list%numberOfFiles=0_int32  
!
  do i=1_int32,size(oswi_list%dat,1)
     call liboswi_clear_type_dat(oswi_list%dat(i),do_deallocate)
  end do
  if (do_deallocate) deallocate(oswi_list%dat)
!
  return
!
!*****************************************************************************80
End subroutine oswi_list_clear


Subroutine oswi_copy ( oswi, copy, dat )
!*****************************************************************************80
!
!! OSWI_COPY 
!
!  Description:
!
!    Copy the entire oswi type to another.
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type ( type_oswi ) oswi, input structure.
!
!    Output, type ( type_oswi ) copy, the duplicate structure.
!
!    Optional input, logical dat, flag to toggle copying the data.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi)  , intent(in )  :: oswi
  type(type_oswi)  , intent(out)  :: copy
  logical, optional, intent(in)   :: dat
!
! Local variables
!
  logical  :: copyData
!
  copyData=.false.
  if (present(dat)) copyData=dat
!
!
! copy oswi input types
!
  call oswi_copy_hdr( oswi%hdr, copy%hdr )
  if (copyData) call oswi_copy_dat( oswi%dat, copy%dat )
!
  return
!
!*****************************************************************************80
End subroutine oswi_copy


Subroutine oswi_stack_copy ( oswi_stack, copy, dat )
!*****************************************************************************80
!
!! OSWI_STACK_COPY 
!
!  Description:
!
!    Copy the entire oswi type to another.
!
!  Modified:
!
!    12 July 2017
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type ( type_oswi_stack ) oswi_stack, input structure.
!
!    Output, type ( type_oswi_stack ) copy, the duplicate structure.
!
!    Optional input, logical dat, flag to toggle copying the data.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_stack), intent(in )  :: oswi_stack
  type(type_oswi_stack), intent(out)  :: copy
  logical, optional    , intent(in)   :: dat
!
! Local variables
!
  logical  :: copyData
!
  copyData=.false.
  if (present(dat)) copyData=dat
!
!
! copy oswi input types
!
  call oswi_copy_hdr( oswi_stack%hdr, copy%hdr )
  if (copyData) call oswi_stack_copy_dat( oswi_stack%dat, copy%dat )
!
  return
!
!*****************************************************************************80
End subroutine oswi_stack_copy


Subroutine oswi_copy_hdr ( hdr, copy, append_history )
!*****************************************************************************80
!
!! OSWI_COPY_HDR
!
!  Description:
!
!    Copy the oswi hdr type to another.
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type ( type_oswi_hdr ) hdr, input structure.
!
!    Output, type ( type_oswi_hdr ) copy, the duplicate structure.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_hdr), intent(in )          :: hdr
  type(type_oswi_hdr), intent(out), target  :: copy
  logical, intent(in), optional             :: append_history
!
  logical :: append_hist
  append_hist=.false.
  if (present(append_history)) append_hist=append_history
!
! copy oswi input types
!
  call oswi_copy_type_env( hdr%env, copy%env )
  call oswi_copy_type_dem( hdr%dem, copy%dem )
  call oswi_copy_type_osw( hdr%osw, copy%osw )
  call oswi_copy_type_grid( hdr%grid, copy%grid )
  call oswi_copy_type_f( hdr%f, copy%f, .not.(hdr%swi.or.hdr%swh) )
!
! out
!
  copy%type=hdr%type
  copy%integrate=hdr%integrate
  copy%fanalysis=hdr%fanalysis
  copy%variance=hdr%variance
  copy%normalize=hdr%normalize
  copy%dB=hdr%dB
  copy%lg=hdr%lg
  copy%Pa=hdr%Pa
  copy%air=hdr%air
  copy%sea=hdr%sea
  copy%bed=hdr%bed
  copy%swh=hdr%swh
  copy%swi=hdr%swi
  copy%sfd=hdr%sfd
!
! general
!
  copy%id=hdr%id
  copy%units=hdr%units
  copy%clip=hdr%clip
  copy%debug=hdr%debug
  copy%global=hdr%global
  copy%name=hdr%name
  copy%long_name=hdr%long_name
  copy%standard_name=hdr%standard_name
  if (append_hist) then
    copy%history=trim(hdr%history)//' | '//trim(copy%history)
  else
    copy%history=hdr%history
  end if
  copy%referencePressure=hdr%referencePressure
  copy%missingValue=hdr%missingValue
!
! set frequency pointer
!
  if (copy%swh.or.copy%swi) then
    copy%f%dat=>copy%f%wave
  else
    copy%f%dat=>copy%f%sound
  end if
!
  return
!
!*****************************************************************************80
End subroutine oswi_copy_hdr


Subroutine oswi_copy_dat ( dat, copy )
!*****************************************************************************80
!
!! OSWI_COPY_DAT
!
!  Description:
!
!    Copy the oswi dat type to another.
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type ( type_oswi_dat ) dat, input structure.
!
!    Output, type ( type_oswi_dat ) copy, the duplicate structure.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_dat), intent(in )  :: dat
  type(type_oswi_dat), intent(out)  :: copy
!
  copy%epoch=dat%epoch
  copy%time=dat%time
  copy%file=dat%file
!
  call liboswi_copy_type_dat_ll ( dat%ll , copy%ll  )
  call liboswi_copy_type_dat_llf( dat%llf, copy%llf )
!
  return
!
!*****************************************************************************80
End subroutine oswi_copy_dat


Subroutine liboswi_copy_type_dat_ll ( dat, copy )
!*****************************************************************************80
!
!! LIBOSWI_COPY_DAT_TYPE_DAT_LL
!
!  Description:
!
!    Copy the oswi dat type to another.
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type ( type_oswi_dat_ll ) dat, input structure.
!
!    Output, type ( type_oswi_dat_ll ) copy, the duplicate structure.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_dat_ll), intent(in )  :: dat
  type(type_oswi_dat_ll), intent(out)  :: copy
!
  copy%maxValue=dat%maxValue
!
  if (allocated(dat%dat)) then
    if (allocated(copy%dat).and.(size(dat%dat,1).ne.size(copy%dat,1))) deallocate(copy%dat)
    if (.not.allocated(copy%dat)) allocate(copy%dat(size(dat%dat,1)))
    copy%dat=dat%dat
  end if
  if (allocated(dat%mask)) then
    if (allocated(copy%mask).and.(size(dat%mask,1).ne.size(copy%mask,1))) deallocate(copy%mask)
    if (.not.allocated(copy%mask)) allocate(copy%mask(size(dat%mask,1)))
    copy%mask=dat%mask
  end if

  if (allocated(dat%fd)) then
    if (allocated(copy%fd).and.(size(dat%fd,1).ne.size(copy%fd,1))) deallocate(copy%fd)
    if (.not.allocated(copy%fd)) allocate(copy%fd(size(dat%fd,1)))
    copy%fd=dat%fd
  end if
  if (allocated(dat%fc)) then
    if (allocated(copy%fc).and.(size(dat%fc,1).ne.size(copy%fc,1))) deallocate(copy%fc)
    if (.not.allocated(copy%fc)) allocate(copy%fc(size(dat%fc,1)))
    copy%fc=dat%fc
  end if
  if (allocated(dat%fr)) then
    if (allocated(copy%fr).and.(size(dat%fr,1).ne.size(copy%fr,1))) deallocate(copy%fr)
    if (.not.allocated(copy%fr)) allocate(copy%fr(size(dat%fr,1)))
    copy%fr=dat%fr
  end if
  if (allocated(dat%fb)) then
    if (allocated(copy%fb).and.(size(dat%fb,1).ne.size(copy%fb,1))) deallocate(copy%fb)
    if (.not.allocated(copy%fb)) allocate(copy%fb(size(dat%fb,1)))
    copy%fb=dat%fb
  end if
!
  return
!
!*****************************************************************************80
End subroutine liboswi_copy_type_dat_ll


Subroutine liboswi_copy_type_dat_llf ( dat, copy )
!*****************************************************************************80
!
!! LIBOSWI_COPY_DAT_TYPE_DAT_LLF
!
!  Description:
!
!    Copy the oswi dat type to another.
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type ( type_oswi_dat_llf ) dat, input structure.
!
!    Output, type ( type_oswi_dat_llf ) copy, the duplicate structure.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_dat_llf), intent(in )  :: dat
  type(type_oswi_dat_llf), intent(out)  :: copy
!
  copy%maxValue=dat%maxValue
!
  if (allocated(dat%dat)) then
    if (allocated(copy%dat).and.(size(dat%dat,1).ne.size(copy%dat,1) &
      .or.size(dat%dat,2).ne.size(copy%dat,2))) deallocate(copy%dat)
    if (.not.allocated(copy%dat)) allocate(copy%dat(size(dat%dat,1),size(dat%dat,2)))
    copy%dat=dat%dat
  end if
  if (allocated(dat%mask)) then
    if (allocated(copy%mask).and.(size(dat%mask,1).ne.size(copy%mask,1) &
      .or.size(dat%mask,2).ne.size(copy%mask,2))) deallocate(copy%mask)
    if (.not.allocated(copy%mask)) allocate(copy%mask(size(dat%mask,1),size(dat%mask,2)))
    copy%mask=dat%mask
  end if
!
  return
!
!*****************************************************************************80
End subroutine liboswi_copy_type_dat_llf


Subroutine oswi_stack_copy_dat ( dat, copy )
!*****************************************************************************80
!
!! OSWI_STACK_COPY_DAT
!
!  Description:
!
!    Copy the oswi dat type to another.
!
!  Modified:
!
!    12 July 2017
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type ( type_oswi_dat ) dat, input structure.
!
!    Output, type ( type_oswi_dat ) copy, the duplicate structure.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_stack_dat), intent(in )  :: dat
  type(type_oswi_stack_dat), intent(out)  :: copy
!
  copy%numberOfFiles=dat%numberOfFiles
  copy%maxValue=dat%maxValue
!
  if (allocated(dat%epoch)) then
    if (allocated(copy%epoch).and.size(dat%epoch,1).ne.size(copy%epoch,1)) deallocate(copy%epoch)
    if (.not.allocated(copy%epoch)) allocate(copy%epoch(size(dat%epoch,1)))
    copy%epoch=dat%epoch
  end if
  if (allocated(dat%w)) then
    if (allocated(copy%w).and.size(dat%w,1).ne.size(copy%w,1)) deallocate(copy%w)
    if (.not.allocated(copy%w)) allocate(copy%w(size(dat%w,1)))
    copy%w=dat%w
  end if
  if (allocated(dat%wf)) then
    if (allocated(copy%wf).and.(size(dat%wf,1).ne.size(copy%wf,1) &
      .or.size(dat%wf,2).ne.size(copy%wf,2))) deallocate(copy%wf)
    if (.not.allocated(copy%wf)) allocate(copy%wf(size(dat%wf,1),size(dat%wf,2)))
    copy%wf=dat%wf
  end if
  if (allocated(dat%mask)) then ! mask
    if (allocated(copy%mask).and.size(dat%mask,1).ne.size(copy%mask,1)) deallocate(copy%mask)
    if (.not.allocated(copy%mask)) allocate(copy%mask(size(dat%mask,1)))
    copy%mask=dat%mask
  end if
!
  call oswi_stack_copy_dat_var(dat%mu,copy%mu)
  call oswi_stack_copy_dat_var(dat%std,copy%std)
  call oswi_stack_copy_dat_var(dat%ci_lb,copy%ci_lb)
  call oswi_stack_copy_dat_var(dat%ci_ub,copy%ci_ub)
!
  return
!
!*****************************************************************************80
End subroutine oswi_stack_copy_dat


Subroutine oswi_stack_copy_dat_var ( var, copy )
!*****************************************************************************80
!
!! OSWI_STACK_COPY_DAT
!
!  Description:
!
!    Copy the oswi dat type to another.
!
!  Modified:
!
!    12 July 2017
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type ( type_oswi_dat ) dat, input structure.
!
!    Output, type ( type_oswi_dat ) copy, the duplicate structure.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_stack_dat_var), intent(in )  :: var
  type(type_oswi_stack_dat_var), intent(out)  :: copy
!
  if (allocated(var%ll)) then
    if (allocated(copy%ll).and.size(copy%ll,1).ne.size(var%ll,1)) deallocate(copy%ll)
    if (.not.allocated(copy%ll)) allocate(copy%ll(size(var%ll,1)))
    copy%ll=var%ll
  end if
  if (allocated(var%fc)) then
    if (allocated(copy%fc).and.size(copy%fc,1).ne.size(var%fc,1)) deallocate(copy%fc)
    if (.not.allocated(copy%fc)) allocate(copy%fc(size(var%fc,1)))
    copy%fc=var%fc
  end if
  if (allocated(var%fd)) then
    if (allocated(copy%fd).and.size(copy%fd,1).ne.size(var%fd,1)) deallocate(copy%fd)
    if (.not.allocated(copy%fd)) allocate(copy%fd(size(var%fd,1)))
    copy%fd=var%fd
  end if
  if (allocated(var%fb)) then
    if (allocated(copy%fb).and.size(copy%fb,1).ne.size(var%fb,1)) deallocate(copy%fb)
    if (.not.allocated(copy%fb)) allocate(copy%fb(size(var%fb,1)))
    copy%fb=var%fb
  end if
  if (allocated(var%fr)) then
    if (allocated(copy%fr).and.size(copy%fr,1).ne.size(var%fr,1)) deallocate(copy%fr)
    if (.not.allocated(copy%fr)) allocate(copy%fr(size(var%fr,1)))
    copy%fr=var%fr
  end if
  if (allocated(var%llf)) then
    if (allocated(copy%llf).and.(size(copy%llf,1).ne.size(var%llf,1)).or.&
      size(copy%llf,2).ne.size(var%llf,2)) deallocate(copy%llf)
    if (.not.allocated(copy%llf)) allocate(copy%llf(size(var%llf,1),size(var%llf,2)))
    copy%llf=var%llf
  end if
!
  return
!
!*****************************************************************************80
End subroutine oswi_stack_copy_dat_var


Subroutine oswi_copy_hdr_essentials ( hdr, copy, verbose, append_history )
!*****************************************************************************80
!
!! OSWI_COPY_HDR_ESSENTIALS
!
!  Description:
!
!    Copy the essential variables from one oswi type to another.
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type ( type_oswi_hdr ) hdr, input structure.
!
!    Output, type ( type_oswi_hdr ) copy, the duplicate structure.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_hdr), intent(in   )  :: hdr
  type(type_oswi_hdr), intent(inout)  :: copy
  logical, optional  , intent(in)     :: verbose
  logical, intent(in), optional       :: append_history
!
! Local variables
!
  logical :: append_hist, verb
  append_hist=.false.
  if (present(append_history)) append_hist=append_history
!
! init
!
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
  if (verb) write (output_unit,"('> ',a)") 'Copy type_oswi_hdr essentials'
!
! copy oswi input types
!
  if (.not.hdr%swi.and..not.hdr%swh) then
    call oswi_copy_type_dem( hdr%dem, copy%dem )
    call oswi_copy_type_env( hdr%env, copy%env )
  end if
  call oswi_copy_type_osw( hdr%osw, copy%osw )
  call oswi_copy_type_grid( hdr%grid, copy%grid )
  call oswi_copy_type_f( hdr%f, copy%f, .not.(hdr%swi.or.hdr%swh) )
!
! general
!
  copy%clip=hdr%clip
  copy%global=hdr%global
  if (append_hist) then
    copy%history=trim(hdr%history)//' | '//trim(copy%history)
  else
    copy%history=hdr%history
  end if
!
  return
!
!*****************************************************************************80
End subroutine oswi_copy_hdr_essentials


Subroutine oswi_copy_dat_essentials ( dat, copy, verbose )
!*****************************************************************************80
!
!! OSWI_COPY_DAT_ESSENTIALS
!
!  Description:
!
!    Copy the essential variables from one oswi type to another.
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type ( type_oswi_dat ) dat, input structure.
!
!    Output, type ( type_oswi_dat ) copy, the duplicate structure.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_dat), intent(in   )  :: dat
  type(type_oswi_dat), intent(inout)  :: copy
  logical, optional  , intent(in)     :: verbose
!
! Local variables
!
  logical  :: verb
!
! init
!
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
  if (verb) write (output_unit,"('> ',a)") 'Copy type_oswi_dat essentials'
!
! copy dat
!
  copy%epoch=dat%epoch
  copy%time=dat%time
  copy%file=dat%file
!
  copy%ll%maxValue=dat%ll%maxValue
  copy%llf%maxValue=dat%llf%maxValue
!
  return
!
!*****************************************************************************80
End subroutine oswi_copy_dat_essentials


Subroutine oswi_copy_essentials ( oswi, copy, verbose )
!*****************************************************************************80
!
!! OSWI_COPY_ESSENTIALS
!
!  Description:
!
!    Copy the essential variables from one oswi type to another.
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type ( type_oswi ) oswi, input structure.
!
!    Output, type ( type_oswi ) copy, the duplicate structure.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi), intent(in   )  :: oswi
  type(type_oswi), intent(inout)  :: copy
  logical, optional  , intent(in) :: verbose
!
! Local variables
!
  logical  :: verb
!
! init
!
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
!
! copy hdr information
!
  call oswi_copy_hdr_essentials ( oswi%hdr, copy%hdr, verb )
!
! copy dat
!
  call oswi_copy_dat_essentials ( oswi%dat, copy%dat, verb )
!
  return
!
!*****************************************************************************80
End subroutine oswi_copy_essentials


Function oswi_compare ( A, B, relax ) result( match )
!*****************************************************************************80
!
!! OSWI_COMPARE 
!
!  Description:
!
!    Compare two oswi types. Optionally, comparison rules can be relaxed.
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type (type_oswi) A, oswi structure A
!
!    Input, type (type_oswi) B, oswi structure B
!
!    Optional input, logical relaxed, boolean to relax comparison
!
!    Output, logical compare_oswi.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi), intent(in)    :: A, B
  logical, intent(in), optional  :: relax
  logical                        :: match
!
  match=oswi_compare_hdr( A%hdr, B%hdr, relax)
!
  return
!
!*****************************************************************************80
End function oswi_compare


Function oswi_compare_hdr ( A, B, relax ) result( match )
!*****************************************************************************80
!
!! OSWI_COMPARE 
!
!  Description:
!
!    Compare two oswi hdr types. Optionally, comparison rules can be relaxed.
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type (type_oswi_hdr) A, oswi header structure A
!
!    Input, type (type_oswi_hdr) B, oswi header structure B
!
!    Optional input, logical relaxed, boolean to relax comparison
!
!    Output, logical compare_oswi.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_hdr), intent(in)  :: A, B
  logical, intent(in), optional  :: relax
  logical                        :: match
!
! Local variables
!
  logical  :: doRelax
!
  match=.false.
  doRelax=.false.
  if (present(relax)) doRelax=relax
!
! copy oswi input types
!
  if (.not.oswi_compare_type_env( A%env, B%env )) return
  if (.not.oswi_compare_type_dem( A%dem, B%dem )) return
  if (.not.oswi_compare_type_osw( A%osw, B%osw )) return
  if (.not.oswi_compare_type_grid( A%grid, B%grid )) return
  if (.not.oswi_compare_type_f( A%f, B%f )) return
!
! out
!
  if (A%type.ne.B%type) return
  if (A%integrate.neqv.B%integrate) return
  if (A%fanalysis.neqv.B%fanalysis) return
  if (A%normalize.neqv.B%normalize) return
  if (A%air.neqv.B%air) return
  if (A%sea.neqv.B%sea) return
  if (A%bed.neqv.B%bed) return
  if (A%swh.neqv.B%swh) return
  if (A%swi.neqv.B%swi) return
  if (A%sfd.neqv.B%sfd) return
!
! general
!
  if (A%clip.neqv.B%clip) return
  if (A%debug.neqv.B%debug) return
  if (A%global.neqv.B%global) return
  if (A%name.ne.B%name) return
!
  if (.not.dorelax) then
    if (A%long_name.ne.B%long_name) return
    if (A%standard_name.ne.B%standard_name) return
    if (A%variance.neqv.B%variance) return
    if (A%dB.neqv.B%dB) return
    if (A%lg.neqv.B%lg) return
    if (A%Pa.neqv.B%Pa) return
!    if (A%id.ne.B%id) return
    if (A%units.ne.B%units) return
!    if (A%referencePressure.ne.B%referencePressure) return
    if (A%missingValue.ne.B%missingValue) return
  end if
!
  match=.true.
!
  return
!
!*****************************************************************************80
End function oswi_compare_hdr


Subroutine oswi_clear_type_grid ( grid, dealloc )
!*****************************************************************************80
!
!! OSWI_CLEAR_TYPE_GRID
!
!  Description:
!
!    Clear and reset the oswi grid type structure. Optionally, array deallocation
!    can be disabled.  
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi_grid ) grid, oswi grid type.
!
!    Optional input, logical DEALLOC, flag to toggle deallocating the arrays.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_grid), intent(inout)  :: grid
  logical, intent(in), optional          :: dealloc
!
! Local variables
!
  logical  :: do_deallocate
!
  do_deallocate=.true.
  if (present(dealloc)) do_deallocate=dealloc
!
  call oswi_clear_type_grid_ll( grid%lat, dealloc )
  call oswi_clear_type_grid_ll( grid%lon, dealloc )
!
  grid%gridType = ''
  grid%interpolated = .false.
  grid%distinctGridValues = .false.
  grid%earthIsOblate = .false.
  grid%numberOfPoints = 0_int32
  grid%earthRadius = 0_int32
  grid%icosahedronEdge = 0_int32
  if (do_deallocate.and.allocated(grid%pl)) deallocate(grid%pl)
!
  return
!
!*****************************************************************************80
End subroutine oswi_clear_type_grid


Subroutine oswi_copy_type_grid ( grid, copy )
!*****************************************************************************80
!
!! OSWI_COPY_TYPE_GRID 
!
!  Description:
!
!    Copy the oswi grid type content.
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type ( type_oswi_grid ) grid, input structure.
!
!    Output, type ( type_oswi_grid ) copy, the duplicate structure.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_grid), intent(in )  :: grid
  type(type_oswi_grid), intent(out)  :: copy
!
  call oswi_copy_type_grid_ll( grid%lat, copy%lat )
  call oswi_copy_type_grid_ll( grid%lon, copy%lon )
!
  copy%gridType = grid%gridType
  copy%interpolated = grid%interpolated
  copy%distinctGridValues = grid%distinctGridValues
  copy%earthIsOblate = grid%earthIsOblate
  copy%earthRadius = grid%earthRadius
  copy%icosahedronEdge = grid%icosahedronEdge
  copy%numberOfPoints = grid%numberOfPoints
  if (allocated(grid%pl)) then
    if (allocated(copy%pl).and.size(copy%pl).ne.size(grid%pl)) deallocate(copy%pl)
    if (.not.allocated(copy%pl)) allocate(copy%pl(size(grid%pl)))
    copy%pl=grid%pl
  end if
!
  return
!
!*****************************************************************************80
End subroutine oswi_copy_type_grid


Function oswi_compare_type_grid ( A, B ) result( match )
!*****************************************************************************80
!
!! OSWI_COMPARE_TYPE_GRID
!
!  Description:
!
!    Compare two oswi gid types.
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type (type_oswi_grid) A, oswi grid structure A
!
!    Input, type (type_oswi_grid) B, oswi grid structure B
!
!    Output, logical compare_oswi_type_grid.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_grid), intent(in)  :: A, B
  logical                           :: match 
!
  match=.false.
!
  if (.not.oswi_compare_type_grid_ll( A%lat, B%lat ) ) return
  if (.not.oswi_compare_type_grid_ll( A%lon, B%lon ) ) return
!
  if (A%gridType.ne.B%gridType) return
  if (A%interpolated.neqv.B%interpolated) return
  if (A%distinctGridValues.neqv.B%distinctGridValues) return
  if (A%earthIsOblate.neqv.B%earthIsOblate) return
  if (A%numberOfPoints.ne.B%numberOfPoints) return
  if (A%earthRadius.ne.B%earthRadius) return
  if (abs(A%icosahedronEdge-B%icosahedronEdge).gt.1.d-8) return
!
  match=.true.
  return
!
!*****************************************************************************80
End function oswi_compare_type_grid



Subroutine oswi_clear_type_grid_ll ( ll, dealloc )
!*****************************************************************************80
!
!! OSWI_CLEAR_TYPE_GRID_LL
!
!  Description:
!
!    Clear and reset the oswi grid_ll type. Optionally, array deallocation
!    can be disabled.  
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi_grid_ll ) ll, oswi grid_ll type.
!
!    Optional input, logical DEALLOC, flag to toggle deallocating the arrays.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_grid_ll), intent(inout)  :: ll
  logical, intent(in), optional             :: dealloc
!
! Local variables
!
  logical  :: do_deallocate
!
  do_deallocate=.true.
  if (present(dealloc)) do_deallocate=dealloc
!
  ll%reversed = .false.
  ll%size = 0_int32
  ll%count = 0_int32
  ll%ixFirst = 0_int32
  ll%ixLast = 0_int32
  ll%first = 0._double
  ll%last = 0._double
  ll%increment = 0._double
  ll%range = 0._double
  if (do_deallocate) then
    if (allocated(ll%dat)) deallocate(ll%dat)
    if (allocated(ll%dat_unpacked)) deallocate(ll%dat_unpacked)
    if (allocated(ll%mask)) deallocate(ll%mask)
  end if
!
  return
!
!*****************************************************************************80
End subroutine oswi_clear_type_grid_ll


Subroutine oswi_copy_type_grid_ll ( ll, copy )
!*****************************************************************************80
!
!! OSWI_COPY_TYPE_GRID_LL 
!
!  Description:
!
!    Copy the oswi grid_ll type content.
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type ( type_oswi_grid_ll ) ll, input structure.
!
!    Output, type ( type_oswi_grid_ll ) copy, the duplicate structure.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_grid_ll), intent(in )  :: ll
  type(type_oswi_grid_ll), intent(out)  :: copy
!
  copy%reversed = ll%reversed
  copy%size = ll%size
  copy%count = ll%count
  copy%ixFirst = ll%ixFirst
  copy%ixLast = ll%ixLast
  copy%first = ll%first
  copy%last = ll%last
  copy%increment = ll%increment
  copy%range = ll%range
  if (allocated(ll%dat)) then
    if (allocated(copy%dat).and.size(copy%dat).ne.size(ll%dat)) deallocate(copy%dat)
    if (.not.allocated(copy%dat)) allocate(copy%dat(size(ll%dat)))
    copy%dat=ll%dat
  end if
  if (allocated(ll%mask)) then
    if (allocated(copy%mask).and.size(copy%mask).ne.size(ll%mask)) deallocate(copy%mask)
    if (.not.allocated(copy%mask)) allocate(copy%mask(size(ll%mask)))
    copy%mask=ll%mask
  end if
  if (allocated(ll%dat_unpacked)) then
    if (allocated(copy%dat_unpacked).and.size(copy%dat_unpacked).ne.size(ll%dat_unpacked)) &
      deallocate(copy%dat_unpacked)
    if (.not.allocated(copy%dat_unpacked)) allocate(copy%dat_unpacked(size(ll%dat_unpacked)))
    copy%dat_unpacked=ll%dat_unpacked
  end if
!
  return
!
!*****************************************************************************80
End subroutine oswi_copy_type_grid_ll


Function oswi_compare_type_grid_ll ( A, B ) result(match)
!*****************************************************************************80
!
!! OSWI_COMPARE 
!
!  Description:
!
!    Compare two oswi grid_ll types.
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type (type_oswi_grid_ll) A, oswi grid_ll structure A
!
!    Input, type (type_oswi_grid_ll) B, oswi grid_ll structure B
!
!    Output, logical match
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_grid_ll), intent(in)  :: A, B
  logical                              :: match
!
  match=.false.
!
  if (A%reversed.neqv.B%reversed) return
  if (A%size.ne.B%size) return
  if (A%count.ne.B%count) return
  if (A%ixFirst.ne.B%ixFirst) return
  if (A%ixLast.ne.B%ixLast) return
  if (abs(A%first-B%first).gt.1.d-8) return
  if (abs(A%last-B%last).gt.1.d-8) return
  if (abs(A%increment-B%increment).gt.1.d-8) return
  if (abs(A%range-B%range).gt.1.d-8) return
!
  match=.true.
  return
!
!*****************************************************************************80
End function oswi_compare_type_grid_ll


Subroutine oswi_clear_type_env ( env )
!*****************************************************************************80
!
!! OSWI_CLEAR_TYPE_ENV
!
!  Description:
!
!    Clear and reset the oswi environment type.  
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi_env ) env, oswi enviroment type.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_env), intent(inout)  :: env
!
  call oswi_clear_type_env_var ( env%rho_air )
  call oswi_clear_type_env_var ( env%rho_sea )
  call oswi_clear_type_env_var ( env%rho_bed )
  call oswi_clear_type_env_var ( env%c_air )
  call oswi_clear_type_env_var ( env%c_sea )
  call oswi_clear_type_env_var ( env%c_bed )
!
  return
!
!*****************************************************************************80
End subroutine oswi_clear_type_env


Subroutine oswi_copy_type_env ( env, copy )
!*****************************************************************************80
!
!! OSWI_COPY_TYPE_ENV
!
!  Description:
!
!    Copy the oswi environment type content.
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type ( type_oswi_env ) env, input structure.
!
!    Output, type ( type_oswi_env ) copy, the duplicate structure.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_env), intent(in )  :: env
  type(type_oswi_env), intent(out)  :: copy
!
! ----
!
  copy=env
!
  return
!
!*****************************************************************************80
End subroutine oswi_copy_type_env


Function oswi_compare_type_env ( A, B ) result( match )
!*****************************************************************************80
!
!! OSWI_COMPARE_TYPE_ENV
!
!  Description:
!
!    Compare two oswi environment types.
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type (type_oswi_env) A, oswi environment structure A
!
!    Input, type (type_oswi_env) B, oswi environment structure B
!
!    Output, logical match.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_env), intent(in)  :: A, B
  logical                          :: match
!
  match=.false.
!
  if (.not.oswi_compare_type_env_var( A%rho_air, B%rho_air ) ) return
  if (.not.oswi_compare_type_env_var( A%rho_sea, B%rho_sea ) ) return
  if (.not.oswi_compare_type_env_var( A%rho_bed, B%rho_bed ) ) return
  if (.not.oswi_compare_type_env_var( A%c_air, B%c_air ) ) return
  if (.not.oswi_compare_type_env_var( A%c_sea, B%c_sea ) ) return
  if (.not.oswi_compare_type_env_var( A%c_bed, B%c_bed ) ) return
!
  match=.true.
  return
!
!*****************************************************************************80
End function oswi_compare_type_env


Subroutine oswi_clear_type_env_var ( var )
!*****************************************************************************80
!
!! OSWI_CLEAR_TYPE_ENV_VAR
!
!  Description:
!
!    Clear and reset the oswi environment variable type.  
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi_env_var ) var, oswi enviroment variable type.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_env_var), intent(inout)  :: var
!
  var%name = ''
  var%long_name = ''
  var%standard_name = ''
  var%units = ''
  var%value = 0._double
!
  return
!
!*****************************************************************************80
End subroutine oswi_clear_type_env_var


Function oswi_compare_type_env_var( A, B ) result( match )
!*****************************************************************************80
!
!! OSWI_COMPARE_TYPE_ENV_VAR
!
!  Description:
!
!    Compare two oswi environment variable types.
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type (type_oswi_env_var) A, oswi environment variable type A
!
!    Input, type (type_oswi_env_var) B, oswi environment variable type B
!
!    Output, logical match.
!
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_env_var), intent(in)  :: A, B
  logical                              :: match
!
  match=.false.
!
  if (A%name.ne.B%name) return
  if (A%long_name.ne.B%long_name) return
  if (A%standard_name.ne.B%standard_name) return
  if (A%units.ne.B%units) return
  if (abs(A%value-B%value).gt.1.d-8) return
!
  match=.true.
  return
!
!*****************************************************************************80
End function oswi_compare_type_env_var


Subroutine oswi_clear_type_f ( f, dealloc )
!*****************************************************************************80
!
!! OSWI_CLEAR_TYPE_F
!
!  Description:
!
!    Clear and reset the oswi frequency type structure. Optionally, array deallocation
!    can be disabled.  
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi_f ) f, oswi frequency type.
!
!    Optional input, logical DEALLOC, flag to toggle deallocating the arrays.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_f), intent(inout)            :: f
  logical          , intent(in)   , optional  :: dealloc
!
! Local variables
!
  logical  :: do_deallocate
!
  do_deallocate=.true.
  if (present(dealloc)) do_deallocate=dealloc
!
  f%size          = 0_int32
  f%count         = 0_int32
  f%ixFirst       = 0_int32
  f%ixLast        = 0_int32
  f%first         = 0._double
  f%last          = 0._double
  if (do_deallocate) then
    if (allocated(f%sound)) deallocate(f%sound)
    if (allocated(f%wave)) deallocate(f%wave)
    if (allocated(f%mask)) deallocate(f%mask)
    if (associated(f%dat)) nullify(f%dat)
  end if
!
  return
!
!*****************************************************************************80
End subroutine oswi_clear_type_f


Subroutine oswi_copy_type_f ( f, copy, sound )
!*****************************************************************************80
!
!! OSWI_COPY_TYPE_F
!
!  Description:
!
!    Copy the oswi frequency type content.
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type ( type_oswi_f ) f, input structure.
!
!    Output, type ( type_oswi_f ) copy, the duplicate structure.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_f), intent(in )          :: f
  type(type_oswi_f), intent(out), target  :: copy
  logical          , intent(in )          :: sound
!
  copy%size          = f%size
  copy%count         = f%count
  copy%ixFirst       = f%ixFirst
  copy%ixLast        = f%ixLast
  copy%first         = f%first
  copy%last          = f%last
  if (allocated(f%sound)) then
    if (allocated(copy%sound).and.size(copy%sound).ne.size(f%sound)) deallocate(copy%sound)
    if (.not.allocated(copy%sound)) allocate(copy%sound(size(f%sound)))
     copy%sound=f%sound
  end if
  if (allocated(f%wave)) then
    if (allocated(copy%wave).and.size(copy%wave).ne.size(f%wave)) deallocate(copy%wave)
    if (.not.allocated(copy%wave)) allocate(copy%wave(size(f%wave)))
     copy%wave=f%wave
  end if
  if (allocated(f%mask)) then
    if (allocated(copy%mask).and.size(copy%mask).ne.size(f%mask)) deallocate(copy%mask)
    if (.not.allocated(copy%mask)) allocate(copy%mask(size(f%mask)))
     copy%mask=f%mask
  end if 
!
  if (sound) then
    copy%dat=>copy%sound
  else
    copy%dat=>copy%wave
  endif
!
  return
!
!*****************************************************************************80
End subroutine oswi_copy_type_f


Function oswi_compare_type_f ( A, B ) result( match )
!*****************************************************************************80
!
!! OSWI_COMPARE_TYPE_F
!
!  Description:
!
!    Compare two oswi frequency types.
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type (type_oswi_f) A, oswi frequency structure A
!
!    Input, type (type_oswi_f) B, oswi frequency structure B
!
!    Output, logical match.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_f), intent(in)  :: A, B
  logical                        :: match
!
  match=.false.
!
  if (A%size.ne.B%size) return
  if (A%count.ne.B%count) return
  if (A%ixFirst.ne.B%ixFirst) return
  if (A%ixLast.ne.B%ixLast) return
  if (abs(A%first-B%first).gt.1.d-8) return
  if (abs(A%last-B%last).gt.1.d-8) return
!
  match=.true.
  return
!
!*****************************************************************************80
End function oswi_compare_type_f


Subroutine oswi_clear_type_osw ( osw )
!*****************************************************************************80
!
!! OSWI_CLEAR_TYPE_OSW
!
!  Description:
!
!    Clear and reset the oswi wave type structure. Optionally, array deallocation
!    can be disabled.  
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi_osw ) osw, oswi wave type.
!
!    Optional input, logical DEALLOC, flag to toggle deallocating the arrays.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_osw), intent(inout)  :: osw
!
  osw%hasselmann=.false.
  osw%directionFullCircle=.false.
  osw%interpolated=.false.
!
  osw%shortName = ''
  osw%units = ''
  osw%centre = ''
  osw%dataClass = ''
  osw%dataType = ''
  osw%dataStream = ''
  osw%gridType = ''
!
  osw%paramId = 0_int32
  osw%experimentVersionNumber = 0_int32
  osw%editionNumber = 0_int32
  osw%directions = 0_int32
  osw%frequencies = 0_int32
  osw%ensembleNumber = 0_int32
  osw%forecastStep = 0_int32
!
  osw%directionIncrement = 0._double
!
  return
!
!*****************************************************************************80
End subroutine oswi_clear_type_osw


Subroutine oswi_copy_type_osw ( osw, copy )
!*****************************************************************************80
!
!! OSWI_COPY_TYPE_OSW
!
!  Description:
!
!    Copy the oswi wave type content.
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type ( type_oswi_osw ) osw, input structure.
!
!    Output, type ( type_oswi_osw ) copy, the duplicate structure.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_osw), intent(in )  :: osw
  type(type_oswi_osw), intent(out)  :: copy
!
  copy=osw
!
  return
!
!*****************************************************************************80
End subroutine oswi_copy_type_osw


Function oswi_compare_type_osw ( A, B ) result( match )
!*****************************************************************************80
!
!! OSWI_COMPARE_TYPE_OSW
!
!  Description:
!
!    Compare two oswi osw types.
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type (type_oswi_osw) A, oswi osw structure A
!
!    Input, type (type_oswi_osw) B, oswi osw structure B
!
!    Output, logical match.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_osw), intent(in)  :: A, B
  logical                          :: match 
!
  match=.false.
!
  if (A%hasselmann.neqv.B%hasselmann) return
  if (A%directionFullCircle.neqv.B%directionFullCircle) return
  if (A%interpolated.neqv.B%interpolated) return
!
  if (A%shortName.ne.B%shortName) return
  if (A%units.ne.B%units) return
  if (A%centre.ne.B%centre) return
  if (A%dataClass.ne.B%dataClass) return
  if (A%dataType.ne.B%dataType) return
  if (A%dataStream.ne.B%dataStream) return
  if (A%gridType.ne.B%gridType) return
!
  if (A%paramId.ne.B%paramId) return
  if (A%experimentVersionNumber.ne.B%experimentVersionNumber) return
  if (A%editionNumber.ne.B%editionNumber) return
  if (A%directions.ne.B%directions) return
  if (A%frequencies.ne.B%frequencies) return
  if (A%forecastStep.ne.B%forecastStep) return
  if (A%ensembleNumber.ne.B%ensembleNumber) return
!
  if (abs(A%directionIncrement-B%directionIncrement).gt.1.d-8) return
!
  match=.true.
  return
!
!*****************************************************************************80
End function oswi_compare_type_osw


Subroutine oswi_clear_type_dem ( dem )
!*****************************************************************************80
!
!! OSWI_CLEAR_TYPE_DEM
!
!  Description:
!
!    Clear and reset the oswi dem type structure. Optionally, array deallocation
!    can be disabled.  
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi_dem ) dem, oswi dem type.
!
!    Optional input, logical DEALLOC, flag to toggle deallocating the arrays.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_dem), intent(inout)  :: dem
!
  dem%defined=.false.
  dem%node_offset=.false.
  dem%useNativeRes=.false.
  dem%interpolated=.false.
  dem%resampleMethod = ''
  dem%title = ''
  dem%institution = ''
  dem%source = ''
  dem%history = ''
  dem%references = ''
  dem%units = ''
!
  return
!
!*****************************************************************************80
End subroutine oswi_clear_type_dem


Subroutine oswi_copy_type_dem ( dem, copy )
!*****************************************************************************80
!
!! OSWI_COPY_TYPE_DEM
!
!  Description:
!
!    Copy the oswi dem type content.
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type ( type_oswi_dem ) dem, input structure.
!
!    Output, type ( type_oswi_dem ) copy, the duplicate structure.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_dem), intent(in )  :: dem
  type(type_oswi_dem), intent(out)  :: copy
!
  copy=dem
!
  return
!
!*****************************************************************************80
End subroutine oswi_copy_type_dem


Function oswi_compare_type_dem ( A, B ) result( match )
!*****************************************************************************80
!
!! OSWI_COMPARE_TYPE_DEM
!
!  Description:
!
!    Compare two oswi dem types.
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type (type_oswi_f) A, oswi dem structure A
!
!    Input, type (type_oswi_f) B, oswi dem structure B
!
!    Output, logical match.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_dem), intent(in)  :: A, B
  logical                          :: match 
!
  match=.false.
! 
  if (A%defined.neqv.B%defined) return
  if (A%defined) then
    if (A%node_offset.neqv.B%node_offset) return
    if (A%useNativeRes.neqv.B%useNativeRes) return
    if (A%interpolated.neqv.B%interpolated) return
    if (A%resampleMethod.ne.B%resampleMethod) return
    if (A%title.ne.B%title) return
    if (A%institution.ne.B%institution) return
    if (A%source.ne.B%source) return
    if (A%history.ne.B%history) return
    if (A%references.ne.B%references) return
    if (A%units.ne.B%units) return
  end if
!
  match=.true.
  return
!
!*****************************************************************************80
End function oswi_compare_type_dem


Subroutine oswi_print_grid_info ( grid, prefix )
!*****************************************************************************80
!
!! OSWI_PRINT_GRID_INFO
!
!  Description:
!
!    Print oswi grid type content to stdout. Optionally the prefix (start) of
!    each output line can be modified. Default prefix equals "'#',1x,".
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, type ( type_oswi_grid ) grid, oswi grid structure.
!
!    Optional input, character(len=*) prefix, prefix string with formatting.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_grid), intent(in)            :: grid
  character(len=*)      , intent(in), optional  :: prefix
!
! Local variables
!
  character(len=len_att) :: pre, fmt_1f, fmt_2f, fmt_1i, fmt_2i, fmt_a, fmt_l
!
  pre="'#',1x,"
  if (present(prefix)) pre=prefix
!
  fmt_a="("//trim(pre)//"2a)"
  fmt_l="("//trim(pre)//"a,l1)"
  fmt_1i="("//trim(pre)//"a,i8)"
  fmt_2i="("//trim(pre)//"a,i8,' (',i8,')')"
  fmt_1f="("//trim(pre)//"a,f8.3)"
  fmt_2f="("//trim(pre)//"a,f8.3,' (',f8.3,')')"
!
  write (output_unit,trim(fmt_a))  'gridType          = ', grid%gridType
  if (grid%icosahedronEdge.gt.1.d-8) write (output_unit,trim(fmt_1f)) &
                      'icosahedronEdge   = ', grid%icosahedronEdge/1000.
  write (output_unit,trim(fmt_1i)) 'numberOfPoints    = ', grid%numberOfPoints
!
  if (allocated(grid%lat%mask)) then
    write (output_unit,trim(fmt_2f)) 'lat_first         = ', grid%lat%first, grid%lat%dat(grid%lat%ixFirst)
    write (output_unit,trim(fmt_2f)) 'lat_last          = ', grid%lat%last, grid%lat%dat(grid%lat%ixLast)
    write (output_unit,trim(fmt_1f)) 'lat_increment     = ', grid%lat%increment
    write (output_unit,trim(fmt_2i)) 'lat_N             = ', grid%lat%size, grid%lat%count
  else
    write (output_unit,trim(fmt_1f)) 'lat_first         = ', grid%lat%first
    write (output_unit,trim(fmt_1f)) 'lat_last          = ', grid%lat%last
    write (output_unit,trim(fmt_1f)) 'lat_increment     = ', grid%lat%increment
    write (output_unit,trim(fmt_1i)) 'lat_N             = ', grid%lat%size
  end if
  write (output_unit,trim(fmt_l))    'lat_reversed      = ', grid%lat%reversed
!
  if (allocated(grid%lon%mask)) then
    write (output_unit,trim(fmt_2f)) 'lon_first         = ', grid%lon%first, grid%lon%dat(grid%lon%ixFirst)
    write (output_unit,trim(fmt_2f)) 'lon_last          = ', grid%lon%last, grid%lon%dat(grid%lon%ixLast)
    write (output_unit,trim(fmt_1f)) 'lon_increment     = ', grid%lon%increment
    write (output_unit,trim(fmt_2i)) 'lon_N             = ', grid%lon%size, grid%lon%count
  else
    write (output_unit,trim(fmt_1f)) 'lon_first         = ', grid%lon%first
    write (output_unit,trim(fmt_1f)) 'lon_last          = ', grid%lon%last
    write (output_unit,trim(fmt_1f)) 'lon_increment     = ', grid%lon%increment
    write (output_unit,trim(fmt_1i)) 'lon_N             = ', grid%lon%size
  end if
  write (output_unit,trim(fmt_l))    'lon_reversed      = ', grid%lon%reversed
!
  return
!
!*****************************************************************************80
End subroutine oswi_print_grid_info


Subroutine liboswi_clear ( verbose )
!*****************************************************************************80
!
!! LIBOSWI_CLEAR
!
!  Description:
!
!    Clear all liboswi allocatable arrays (private and public)
!
!  Modified:
!
!    16 November 2016
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input, logical verbose 
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  logical, intent(in), optional  :: verbose
!
! Local variables
!
  logical  :: verb
!
! init
!
  verb=liboswi_verbose_default
  if (present(verbose)) verb=verbose
!
! Icosahedron interpolation
!
  if (allocated(icosa_iconvex)) deallocate(icosa_iconvex)
  if (allocated(icosa_mask)) deallocate(icosa_mask)
  if (allocated(icosa_alpha)) deallocate(icosa_alpha)
  if (allocated(icosa_beta)) deallocate(icosa_beta)
!
! Modulation coefficients
!
  if (allocated(oswi_mod_coeff_f)) deallocate(oswi_mod_coeff_f)
  if (allocated(oswi_mod_coeff_fd)) deallocate(oswi_mod_coeff_fd)
!
  return
!
!*****************************************************************************80
End subroutine liboswi_clear
