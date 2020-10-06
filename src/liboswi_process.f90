Subroutine oswi_modulate_spectrum ( oswi, verbose )
!*****************************************************************************80
!
!! OSWI_MODULATE_SPECTRUM 
!
!  Description:
!
!    Modulate swi (Hasselmann) with pre-calculated resonance coefficients for
!    the defined output medium/type.  
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
!    Optional input, logical verbose, flag to toggle verbose.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi), intent(inout), target   :: oswi
  logical        , intent(in)              :: verbose
!
! Local variables
!
  integer(int32)  :: k
  real(double), dimension(:), pointer  :: dat_ll
!
!  ---
!
  if (oswi%hdr%swi) return
  if (verbose) write(output_unit,"('> ',2a)") 'Modulate source intensity spectrum'
!
  if (oswi%hdr%swh) then
!        add scalar to SWH variance: H_s**2 = 16 * F(f)
    oswi%dat%llf%dat = 16*oswi%dat%llf%dat
  else
    if (oswi%hdr%dem%defined) then
      oswi%dat%llf%dat=oswi%dat%llf%dat*oswi_mod_coeff_fd
    else
      do k=1_int32,oswi%hdr%f%count
        dat_ll=>oswi%dat%llf%dat(:,k)
        dat_ll=dat_ll*oswi_mod_coeff_f(k)
      end do
    end if
  end if
!
  return
!
!*****************************************************************************80
End subroutine oswi_modulate_spectrum


Subroutine oswi_spectrum_integrate_center_frequency ( oswi, verbose )
!*****************************************************************************80
!
!! OSWI_SPECTRUM_INTEGRATE_CENTER_FREQUENCY
!
!  Description:
!
!    Integrate spectrum and determine center frequency of oswi spectral data.
!
!  Modified:
!
!    25 January 2017
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi ) oswi.
!
!    Optional input, logical verbose, flag to toggle verbose.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi), intent(inout), target   :: oswi
  logical        , intent(in)              :: verbose
!
! Local variables
!
  integer(int32)                       :: i, k
  real(double)                         :: WPower, Power
  real(double), pointer                :: P, Pmax
  real(double), pointer, dimension(:)  :: f
  real(double), dimension(oswi%hdr%f%size) :: df
!
!  ---
!
  if (.not.oswi%hdr%fanalysis) return
  if (verbose) write(output_unit,"('> ',2a)") 'Integrate spectrum and calculate center frequency'
!
! Initialize
!
  call liboswi_allocate_type_dat_ll( oswi%hdr, oswi%dat%ll, .false. )
!
  oswi%dat%ll%mask=any(oswi%dat%llf%mask,dim=2)
  oswi%dat%ll%dat=0._double
  oswi%dat%ll%fd=0._double
  oswi%dat%ll%fc=0._double
!
! Set frequency
!
  if (oswi%hdr%swh.or.oswi%hdr%swi) then
    f => oswi%hdr%f%wave
  else
    f => oswi%hdr%f%sound
  endif
!
! get df
!
  do k=1_int32,oswi%hdr%f%size
    df(k) = f(k) * ( 1.1_double**(.5_double) - 1.1_double**(-.5_double))
  enddo
!
! calculate center frequency and total power
!
  do i=1_int32,oswi%hdr%grid%numberOfPoints
    if (.not.oswi%dat%ll%mask(i)) cycle
    WPower=0._double
    Power=0._double
    Pmax=>oswi%dat%llf%dat(i,oswi%hdr%f%ixFirst)
    do k=oswi%hdr%f%ixFirst, oswi%hdr%f%ixLast
      P => oswi%dat%llf%dat(i,k)
      WPower = WPower + df(k) * f(k) * P
      Power = Power + df(k) * P
      if (P.gt.Pmax) then
        Pmax=>oswi%dat%llf%dat(i,k)
        oswi%dat%ll%fd(i)=f(k) 
      end if
    end do
    if (WPower.gt.0._double) then
      oswi%dat%ll%mask(i)=.true.
      oswi%dat%ll%fc(i)=WPower/Power
      oswi%dat%ll%dat(i)=Power
      ! if (spectralpeak) then oswi%dat%ll(i)=Pmax; else oswi%dat%ll(i)=Power; end if 
    else
      oswi%dat%ll%mask(i)=.false.
    end if
  end do
!
  return
!
!*****************************************************************************80
End subroutine oswi_spectrum_integrate_center_frequency


Subroutine oswi_spectrum_bandwidth ( oswi, verbose )
!*****************************************************************************80
!
!! OSWI_SPECTRUM_BANDWIDTH
!
!  Description:
!
!    Calculate bandwith and rms frequency of spectrum wrt center frequency.
!
!  Modified:
!
!    25 January 2017
!
!  Author:
!
!    Pieter Smets
!
!  Parameters:
!
!    Input/output, type ( type_oswi ) oswi.
!
!    Optional input, logical verbose, flag to toggle verbose.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi), intent(inout), target   :: oswi
  logical        , intent(in)              :: verbose
!
! Local variables
!
  integer(int32)                       :: i, k
  real(double)                         :: f0 
  real(double)                         :: WPower, Power
  real(double), pointer                :: P, fc, bw
  real(double), pointer, dimension(:)  :: f
  real(double), dimension(oswi%hdr%f%size) :: df
!
!  ---
!
  if (.not.oswi%hdr%fanalysis) return
  if (.not.allocated(oswi%dat%ll%fc).or..not.allocated(oswi%dat%ll%mask)) return
  if (verbose) write(output_unit,"('> ',2a)") 'Get spectral bandwidth and rms frequency'
!
! Initialize (only bw and rms)
!
  call liboswi_allocate_type_dat_ll( oswi%hdr, oswi%dat%ll, .false. )
!
  oswi%dat%ll%fb=0._double
  oswi%dat%ll%fr=0._double
!
! Set frequency
!
  if (oswi%hdr%swh.or.oswi%hdr%swi) then
    f => oswi%hdr%f%wave
  else
    f => oswi%hdr%f%sound
  endif
!
! get df
!
  do k=1_int32,oswi%hdr%f%size
    df(k) = f(k) * ( 1.1_double**(.5_double) - 1.1_double**(-.5_double))
  enddo
!
! calculate rms frequency and bandwidth
!
  do i=1_int32,oswi%hdr%grid%numberOfPoints
    if (.not.oswi%dat%ll%mask(i)) cycle
    fc=>oswi%dat%ll%fc(i)
    bw=>oswi%dat%ll%fb(i)
    WPower=0._double
    Power=0._double
    do k=oswi%hdr%f%ixFirst, oswi%hdr%f%ixLast
      P => oswi%dat%llf%dat(i,k)
      WPower = WPower + df(k) * ((f(k)-fc)**2) * P
      Power = Power + df(k) * P
    end do
    bw=sqrt(WPower/Power)
    oswi%dat%ll%fr(i)=sqrt(fc**2+bw**2)
  end do
!
  return
!
!*****************************************************************************80
End subroutine oswi_spectrum_bandwidth


Subroutine oswi_integrate_spectrum ( oswi, verbose )
!*****************************************************************************80
!
!! OSWI_INTEGRATE_SPECTRUM 
!
!  Description:
!
!    Integrate oswi data spectrum for the defined frequency range.
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
!    Optional input, logical verbose, flag to toggle verbose.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi), intent(inout), target   :: oswi
  logical        , intent(in)              :: verbose
!
! Local variables
!
  integer(int32) :: k
  real(double)   :: df
  real(double), dimension(:), pointer  :: P, f
!
!  ---
!
  if (.not.oswi%hdr%integrate) return
!
! Initialize
!
  if (verbose) write(output_unit,"('> ',2a)") 'Integrate frequency spectrum'
!
  call liboswi_allocate_type_dat_ll( oswi%hdr, oswi%dat%ll, .false. )
!
  oswi%dat%ll%dat=0._double
  oswi%dat%ll%mask=.false.
!
! Set frequency
!
  if (oswi%hdr%swh.or.oswi%hdr%swi) then
    f => oswi%hdr%f%wave
  else
    f => oswi%hdr%f%sound
  endif
!
! Integrate spectral density (Power/Hz)
!
  do k=oswi%hdr%f%ixFirst, oswi%hdr%f%ixLast
    df = f(k) * ( 1.1_double**(.5_double) - 1.1_double**(-.5_double))
    P => oswi%dat%llf%dat(:,k)
    oswi%dat%ll%dat = oswi%dat%ll%dat + df * P
  end do
!
! Set mask
!
  oswi%dat%ll%mask=any(oswi%dat%llf%mask,dim=2)
!
  return
!
!*****************************************************************************80
End subroutine oswi_integrate_spectrum


Subroutine oswi_spectral_peak ( oswi, verbose )
!*****************************************************************************80
!
!! OSWI_SPECTRAL_PEAK
!
!  Description:
!
!    Get peak location of oswi spectral data within the defined frequency range.
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
!    Optional input, logical verbose, flag to toggle verbose.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi), intent(inout), target   :: oswi
  logical        , intent(in)              :: verbose
!
  if (oswi%hdr%integrate) return
!
! Initialize
!
  if (verbose) write(output_unit,"('> ',2a)") 'Get spectral peak'
!
! Spectral peak index
!
  call liboswi_allocate_type_dat_ll( oswi%hdr, oswi%dat%ll, .false. )
!
!$omp parallel
  call oswi_spectral_peak_amplitude( oswi, verbose=.false. )
  call oswi_spectral_peak_frequency( oswi, verbose=.false. )
!$omp end parallel
!
  return
!
!*****************************************************************************80
End subroutine oswi_spectral_peak


Subroutine oswi_spectral_peak_frequency ( oswi, verbose )
!*****************************************************************************80
!
!! OSWI_SPECTRAL_PEAK_FREQUENCY
!
!  Description:
!
!    Get dominant frequency of the oswi peak spectral location.
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
!    Optional input, logical verbose, flag to toggle verbose.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi), intent(inout), target   :: oswi
  logical        , intent(in)              :: verbose
!
! Local variables
!
  integer(int32)  :: i, imax
  logical     , pointer, dimension(:)  :: mask
  real(double), pointer, dimension(:)  :: llf, f
!
  if (oswi%hdr%integrate) return
!
! Initialize
!
  if (verbose) write(output_unit,"('> ',2a)") 'Get spectral peak frequency'
!
  if (allocated(oswi%dat%ll%fd).and.size(oswi%dat%ll%fd).ne.oswi%hdr%grid%numberOfPoints) &
    deallocate(oswi%dat%ll%fd)
  if (.not.allocated(oswi%dat%ll%fd)) allocate(oswi%dat%ll%fd(oswi%hdr%grid%numberOfPoints))
!
  oswi%dat%ll%fd=0._double
!
! Set frequency
!
  if (oswi%hdr%swh.or.oswi%hdr%swi) then
    f => oswi%hdr%f%wave(oswi%hdr%f%ixFirst:oswi%hdr%f%ixLast)
  else
    f => oswi%hdr%f%sound(oswi%hdr%f%ixFirst:oswi%hdr%f%ixLast)
  endif
!
! loop over all points
!
  do i=1_int32,oswi%hdr%grid%numberofpoints
    mask=>oswi%dat%llf%mask(i,oswi%hdr%f%ixFirst:oswi%hdr%f%ixLast)
    if (any(mask)) then
      llf=>oswi%dat%llf%dat(i,oswi%hdr%f%ixFirst:oswi%hdr%f%ixLast)
      imax=maxloc(llf,dim=1)
      oswi%dat%ll%fd(i)=f(imax)
    end if
  end do
!
  return
!
!*****************************************************************************80
End subroutine oswi_spectral_peak_frequency


Subroutine oswi_spectral_peak_amplitude ( oswi, verbose )
!*****************************************************************************80
!
!! OSWI_SPECTRAL_PEAK_AMPLITUDE
!
!  Description:
!
!    Get amplitude at the dominant frequency of the oswi spectrum.
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
!    Optional input, logical verbose, flag to toggle verbose.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi), intent(inout), target   :: oswi
  logical        , intent(in)              :: verbose
!
  if (oswi%hdr%integrate) return
!
! Initialize
!
  if (verbose) write(output_unit,"('> ',2a)") 'Get spectral peak amplitude'
!
  if (allocated(oswi%dat%ll%dat).and.size(oswi%dat%ll%dat).ne.oswi%hdr%grid%numberOfPoints) &
    deallocate(oswi%dat%ll%dat)
  if (.not.allocated(oswi%dat%ll%dat)) allocate(oswi%dat%ll%dat(oswi%hdr%grid%numberOfPoints))
!
  oswi%dat%ll%dat = maxval(oswi%dat%llf%dat,dim=1,mask=oswi%dat%llf%mask) 
!
  return
!
!*****************************************************************************80
End subroutine oswi_spectral_peak_amplitude


Subroutine oswi_modify_output ( oswi, verbose )
!*****************************************************************************80
!
!! OSWI_MODIFY_OUTPUT 
!
!  Description:
!
!    Convert or modify oswi data unit.
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
!    Optional input, logical verbose, flag to toggle verbose.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi), intent(inout), target   :: oswi
  logical        , intent(in)              :: verbose
!
!  ---
!
! if (verbose) write(output_unit,"('> ',a)") 'Convert oswi output data'
!
  if (oswi%hdr%integrate) then
!
    call oswi_convert_dat_ll_output ( hdr=oswi%hdr, dat_ll=oswi%dat%ll, verbose=verbose )
!
  else
!
    call oswi_convert_dat_llf_output ( hdr=oswi%hdr, dat_llf=oswi%dat%llf, verbose=verbose )
!
  end if
!
  return
!
!*****************************************************************************80
End subroutine oswi_modify_output


Subroutine oswi_convert_dat_ll_output ( hdr, dat_ll, verbose )
!*****************************************************************************80
!
!! OSWI_CONVERT_DAT_LL_OUTPUT 
!
!  Description:
!
!    Convert or modify oswi data unit.
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
!  Parameters:
!
!    Input       , type ( type_oswi_hdr    ) hdr.
!    Input/output, type ( type_oswi_dat_ll ) dat_ll.
!
!    Optional input, logical verbose, flag to toggle verbose.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_hdr)   , intent(in)   , target   :: hdr
  type(type_oswi_dat_ll), intent(inout), target   :: dat_ll
  logical               , intent(in)              :: verbose
!
! Local variables
!
  real(double)   :: norm, p_shift, p_scale
!
!  ---
!
  if (verbose) write(output_unit,"('> ',a)") 'Convert oswi ll output data'
!
  norm = 1._double
  if (hdr%dB) then
    p_scale = 10._double ! data is still squared!
    p_shift = 20._double*(log10(1._double/hdr%referencePressure)+6._double) ! reference pressure in µPa
  else
    p_scale = 1._double
    if (hdr%lg.and..not.hdr%variance) p_scale = 0.5_double
    p_shift = 0._double
  end if
!
  where (dat_ll%dat.lt.1.d-12)
    dat_ll%mask=.false.
  end where
!
  dat_ll%maxValue=maxval(dat_ll%dat,dat_ll%mask)
!
  if (hdr%Pa.and.(hdr%dB.or.hdr%lg)) then
    if (hdr%normalize) norm = 1._double/( p_scale*log10(dat_ll%maxValue) + p_shift )
    where (dat_ll%mask)
      dat_ll%dat = ( p_scale*log10(dat_ll%dat) + p_shift ) * norm
    end where
  else
    if (hdr%normalize) dat_ll%dat = dat_ll%dat/dat_ll%maxValue
    if (.not.hdr%variance) dat_ll%dat = sqrt(dat_ll%dat)
  end if
!
  return
!
!*****************************************************************************80
End subroutine oswi_convert_dat_ll_output


Subroutine oswi_convert_dat_llf_output ( hdr, dat_llf, verbose )
!*****************************************************************************80
!
!! OSWI_CONVERT_DAT_LLF_OUTPUT 
!
!  Description:
!
!    Convert or modify oswi data unit.
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
!  Parameters:
!
!    Input       , type ( type_oswi_hdr     ) hdr.
!    Input/output, type ( type_oswi_dat_llf ) dat_llf.
!
!    Optional input, logical verbose, flag to toggle verbose.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_hdr)    , intent(in)   , target   :: hdr
  type(type_oswi_dat_llf), intent(inout), target   :: dat_llf
  logical                , intent(in)              :: verbose
!
! Local variables
!
  real(double)   :: norm, p_shift, p_scale
!
!  ---
!
  if (verbose) write(output_unit,"('> ',a)") 'Convert oswi llf output data'
!
  norm = 1._double
  if (hdr%dB) then
    p_scale = 10._double ! data is still squared!
    p_shift = 20._double*(log10(1._double/hdr%referencePressure)+6._double) ! reference pressure in µPa
  else
    p_scale = 1._double
    if (hdr%lg.and..not.hdr%variance) p_scale = 0.5_double
    p_shift = 0._double
  end if
!
  where (dat_llf%dat.lt.1.d-12)
    dat_llf%mask=.false.
  end where
!
  dat_llf%maxValue=maxval(dat_llf%dat,dat_llf%mask)
!
  if (hdr%Pa.and.(hdr%dB.or.hdr%lg)) then
    if (hdr%normalize) norm = 1._double/( p_scale*log10(dat_llf%maxValue) + p_shift )
    where (dat_llf%mask)
      dat_llf%dat = ( p_scale*log10(dat_llf%dat) + p_shift ) * norm
    end where
  else
    if (hdr%normalize) dat_llf%dat = dat_llf%dat/dat_llf%maxValue
    if (.not.hdr%variance) dat_llf%dat = sqrt(dat_llf%dat)
  end if
!
  return
!
!*****************************************************************************80
End subroutine oswi_convert_dat_llf_output


Subroutine oswi_convert_dat_ll_input ( hdr, dat_ll, verbose )
!*****************************************************************************80
!
!! OSWI_CONVERT_DAT_LLF_INPUT 
!
!  Description:
!
!    Convert oswi input data to standard unit.
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
!    Input       , type ( type_oswi_hdr     ) hdr.
!    Input/output, type ( type_oswi_dat_llf ) dat_ll.
!
!    Optional input, logical verbose, flag to toggle verbose.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_hdr)   , intent(in)   , target   :: hdr
  type(type_oswi_dat_ll), intent(inout), target   :: dat_ll
  logical               , intent(in)              :: verbose
!
! Local variables
!
  real(double)   :: norm, p_shift, p_scale
!
!  ---
!
  if (verbose) write(output_unit,"(3x,'>>> ',a)") 'Convert oswi ll input data to base units'
!
! Get mask
!
  dat_ll%mask = abs(dat_ll%dat-hdr%missingValue).gt.1.d-8
!
! Convert to base units
!
  norm = 1._double
  if (hdr%dB) then
    p_scale = 10._double ! data is still squared!
    p_shift = 20._double*(log10(1._double/hdr%referencePressure)+6._double) ! reference pressure in µPa
  else
    p_scale = 1._double
    if (hdr%lg.and..not.hdr%variance) p_scale = 0.5_double
    p_shift = 0._double
  end if
!
  if (hdr%Pa.and.(hdr%dB.or.hdr%lg)) then
    if (hdr%normalize) norm = p_scale*log10(dat_ll%maxValue) + p_shift
    where (dat_ll%mask)
      dat_ll%dat = 10._double**( (dat_ll%dat * norm - p_shift ) / p_scale )
    elsewhere
      dat_ll%dat = 0._double
    end where
  else
    where (.not.dat_ll%mask) dat_ll%dat = 0._double
    if (.not.hdr%variance) then
      dat_ll%dat = dat_ll%dat**2
      if (hdr%normalize) dat_ll%dat = dat_ll%dat*(dat_ll%maxValue**2)
    else
      if (hdr%normalize) dat_ll%dat = dat_ll%dat*dat_ll%maxValue
    end if
!
  end if
!
! Update mask
!
  where (dat_ll%dat.lt.1.d-12)
    dat_ll%mask=.false.
  end where
!
  return
!
!*****************************************************************************80
End subroutine oswi_convert_dat_ll_input


Subroutine oswi_convert_dat_llf_input ( hdr, dat_llf, verbose )
!*****************************************************************************80
!
!! OSWI_CONVERT_DAT_LLF_INPUT 
!
!  Description:
!
!    Convert oswi input data to standard unit.
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
!    Input       , type ( type_oswi_hdr     ) hdr.
!    Input/output, type ( type_oswi_dat_llf ) dat_llf.
!
!    Optional input, logical verbose, flag to toggle verbose.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi_hdr)    , intent(in)   , target   :: hdr
  type(type_oswi_dat_llf), intent(inout), target   :: dat_llf
  logical                , intent(in)              :: verbose
!
! Local variables
!
  real(double)   :: norm, p_shift, p_scale
!
!  ---
!
  if (verbose) write(output_unit,"(3x,'>>> ',a)") 'Convert oswi llf input data to base units'
!
! Get mask
!
  dat_llf%mask = abs(dat_llf%dat-hdr%missingValue).gt.1.d-8
!
! Convert to base units
!
  norm = 1._double
  if (hdr%dB) then
    p_scale = 10._double ! data is still squared!
    p_shift = 20._double*(log10(1._double/hdr%referencePressure)+6._double) ! reference pressure in µPa
  else
    p_scale = 1._double
    if (hdr%lg.and..not.hdr%variance) p_scale = 0.5_double
    p_shift = 0._double
  end if
!
  if (hdr%Pa.and.(hdr%dB.or.hdr%lg)) then
    if (hdr%normalize) norm = p_scale*log10(dat_llf%maxValue) + p_shift
    where (dat_llf%mask)
      dat_llf%dat = 10._double**( (dat_llf%dat * norm - p_shift ) / p_scale )
    elsewhere
      dat_llf%dat = 0._double
    end where
  else
    where (.not.dat_llf%mask) dat_llf%dat = 0._double
    if (.not.hdr%variance) then
      dat_llf%dat = dat_llf%dat**2
      if (hdr%normalize) dat_llf%dat = dat_llf%dat*(dat_llf%maxValue**2)
    else
      if (hdr%normalize) dat_llf%dat = dat_llf%dat*dat_llf%maxValue
    end if
  end if
!
! Update mask
!
  where (dat_llf%dat.lt.1.d-12)
    dat_llf%mask=.false.
  end where
!
  return
!
!*****************************************************************************80
End subroutine oswi_convert_dat_llf_input


Subroutine oswi_mask_output ( oswi, verbose )
!*****************************************************************************80
!
!! OSWI_MASK_OUTPUT 
!
!  Description:
!
!    Convert or modify oswi data unit.
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
!    Optional input, logical verbose, flag to toggle verbose.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi), intent(inout), target   :: oswi
  logical        , intent(in)              :: verbose
!
!  ---
!
  if (verbose) write(output_unit,"('> ',a)") 'Mask output with missing_value'
!
  if (oswi%hdr%integrate) then
!
!   add missingvalue to frequency stuff
!
    if (oswi%hdr%fanalysis) then
!$omp workshare
      where (.not.oswi%dat%ll%mask) 
        oswi%dat%ll%dat = real(oswi%hdr%missingValue,kind=double)
        oswi%dat%ll%fd  = real(oswi%hdr%missingValue,kind=double)
        oswi%dat%ll%fc  = real(oswi%hdr%missingValue,kind=double)
        oswi%dat%ll%fb  = real(oswi%hdr%missingValue,kind=double)
        oswi%dat%ll%fr  = real(oswi%hdr%missingValue,kind=double)
      end where
!$omp end workshare
    else
!$omp workshare
      where (.not.oswi%dat%ll%mask) 
        oswi%dat%ll%dat = real(oswi%hdr%missingValue,kind=double)
      end where
!$omp end workshare
    end if
!
  else
!
!$omp workshare
    where (.not.oswi%dat%llf%mask) 
      oswi%dat%llf%dat = real(oswi%hdr%missingValue,kind=double)
    end where
!$omp end workshare
!
  end if
!
  return
!
!*****************************************************************************80
End subroutine oswi_mask_output


Subroutine oswi_demask_input ( oswi, verbose )
!*****************************************************************************80
!
!! OSWI_DEMASK_INPUT 
!
!  Description:
!
!    Convert or modify oswi data unit.
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
!    Optional input, logical verbose, flag to toggle verbose.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi), intent(inout), target   :: oswi
  logical        , intent(in)              :: verbose
!
!  ---
!
  if (verbose) write(output_unit,"('> ',a)") 'Demask missing_value from input'
!
  if (oswi%hdr%integrate) then
!
!   add missingvalue to frequency stuff
!
    if (oswi%hdr%fanalysis) then
!$omp workshare
      where (.not.oswi%dat%ll%mask) 
        oswi%dat%ll%dat = 0._double
        oswi%dat%ll%fd  = 0._double
        oswi%dat%ll%fc  = 0._double
        oswi%dat%ll%fb  = 0._double
        oswi%dat%ll%fr  = 0._double
      end where
!$omp end workshare
    else
!$omp workshare
      where (.not.oswi%dat%ll%mask) 
        oswi%dat%ll%dat = 0._double
      end where
!$omp end workshare
    end if
!
  else
!
!$omp workshare
    where (.not.oswi%dat%llf%mask) 
      oswi%dat%llf%dat = 0._double
    end where
!$omp end workshare
!
  end if
!
  return
!
!*****************************************************************************80
End subroutine oswi_demask_input


Subroutine set_modulation_coefficients_infinite_ocean_depth ( oswi ) 
!*****************************************************************************80
!
!! SET_MODULATION_COEFFICIENTS_INFINITE_OCEAN_DEPTH 
!
!  Description:
!
!    Set the modulation coefficients for a product with infinite ocean depth. 
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
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi), intent(inout), target   :: oswi
!
! Local variables
!
  integer(int32)  :: k
  real(double)    :: factor, factor1, factor2
  real(double), pointer  :: f, rho_sea, rho_air, c_sea, c_air
!
!  ---
!
!....Initialize
!
  if (allocated(oswi_mod_coeff_f).and.size(oswi_mod_coeff_f,1).ne.oswi%hdr%f%size) &
    deallocate(oswi_mod_coeff_f)
  if (.not.allocated(oswi_mod_coeff_f)) allocate( oswi_mod_coeff_f(oswi%hdr%f%size) )
  oswi_mod_coeff_f = 0._double
!
  rho_sea => oswi%hdr%env%rho_sea%value
  c_sea   => oswi%hdr%env%c_sea%value
  rho_air => oswi%hdr%env%rho_air%value
  c_air   => oswi%hdr%env%c_air%value
!
! Atmosphere
!
  if (oswi%hdr%air) then
!
    factor  = 4 * g**2 * pi**4 * (rho_air/c_air)**2
    factor1 = ( 9 * g**2 ) / ( 4 * pi**2 * c_air**2 )
    factor2 = (c_air/c_sea)**2
!
    do k=1_int32,oswi%hdr%f%size
      f => oswi%hdr%f%sound(k)
      oswi_mod_coeff_f(k) = factor * f**3 * ( factor1 / f**2 + factor2 ) 
    end do
!
! Sea / ocean
!
  elseif (oswi%hdr%sea) then
!
    factor  = 4 * g**2 * pi**4 * (rho_sea/c_sea)**2
    factor1 = 0._double
    factor2 = 1._double
!
    do k=1_int32,oswi%hdr%f%size 
      f     => oswi%hdr%f%sound(k)
      oswi_mod_coeff_f(k) = factor * f**3
    end do
!
  end if
!
! Debug? Copy coefficients to global map
!
  if (oswi%hdr%debug) then
    do k=1_int32,oswi%hdr%f%size
      oswi%dat%llf%dat(:,k)=oswi_mod_coeff_f(k)
    end do
  end if
!
  return
!
!*****************************************************************************80
End subroutine set_modulation_coefficients_infinite_ocean_depth


Subroutine set_modulation_coefficients_finite_ocean_depth ( oswi, depth )
!*****************************************************************************80
!
!! SET_MODULATION_COEFFICIENTS_FINITE_OCEAN_DEPTH 
!
!  Description:
!
!    Set the modulation coefficients for a product with finite ocean depth. 
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
!    Input, real( single ) depth, vector of length numberofpoints specifying
!    ocean depth.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  type(type_oswi), intent(inout), target    :: oswi
  real(single), dimension(oswi%hdr%grid%numberOfPoints), intent(in), target  :: depth
!
! Local variables
!
  integer(int32) :: i, k
  real(double)   :: A, B, C, kpi, c2, factor, factor1, factor2, factor3, R, R2, T, T2
  logical, dimension(oswi%hdr%grid%numberOfPoints), target  :: mask_depth
  logical     , pointer  :: m
  real(single), pointer  :: z
  real(double), pointer  :: f, rho_sea, rho_air, rho_bed, c_sea, c_air, c_bed
!
!  ---
!
!....Initialize
!
  if (allocated(oswi_mod_coeff_fd).and.(size(oswi_mod_coeff_fd,1).ne.oswi%hdr%grid%numberOfPoints &
    .or.size(oswi_mod_coeff_fd,2).ne.oswi%hdr%f%size)) deallocate(oswi_mod_coeff_fd)
  if (.not.allocated(oswi_mod_coeff_fd)) &
    allocate( oswi_mod_coeff_fd( oswi%hdr%grid%numberOfPoints, oswi%hdr%f%size) )
!
  oswi_mod_coeff_fd=0._double
  mask_depth=depth.gt.0._double
!
  rho_sea => oswi%hdr%env%rho_sea%value
  c_sea   => oswi%hdr%env%c_sea%value
  rho_air => oswi%hdr%env%rho_air%value
  c_air   => oswi%hdr%env%c_air%value
  rho_bed => oswi%hdr%env%rho_bed%value
  c_bed   => oswi%hdr%env%c_bed%value
!
! Reflection coefficient
!
  R  = (rho_sea*c_sea)/(rho_bed*c_bed)
  R2 = R**2
!
! Transmission coefficient
!
  T  = 1._double - R
  T2 = T**2
!
! Sea floor deformation (Longuet-Higgins, 1950)
!
  if (oswi%hdr%sfd) then
!
    factor = 2 * ( pi * g * rho_sea / rho_bed )**2 / c_bed**5 * twopi**3
!
    do i=1_int32,oswi%hdr%grid%numberOfPoints
      m=>mask_depth(i)
      if (.not.m) cycle
      z=>depth(i)
      do k=1_int32,oswi%hdr%f%size
        f => oswi%hdr%f%sound(k)
        call calc_modes( f=f,depth=z,beta=c_bed,c=c2 ) ! Kedar error, beta=c_sea ??!!
        oswi_mod_coeff_fd(i,k) = f*c2*factor
      end do
    end do
!
! Sea / ocean
!
  elseif (oswi%hdr%sea) then
!
    factor = 4 * g**2 * pi**4 * (rho_sea/c_sea)**2
!
    do i=1_int32,oswi%hdr%grid%numberOfPoints
      m=>mask_depth(i)
      if (.not.m) cycle
      z=>depth(i)
      do k=1_int32,oswi%hdr%f%size
        f     => oswi%hdr%f%sound(k)
        kpi   = twopi * f * real(abs(z),double) / c_sea
        call get_ocean_resonance( kpi, R2, A, B, C )
        oswi_mod_coeff_fd(i,k) = factor * f**3 * (A/B)
      end do
    end do
!
! Atmosphere
!
  elseif (oswi%hdr%air) then
!
    factor  = 4 * g**2 * pi**4 * (rho_air/c_air)**2
    factor1 = ( 9 * g**2 ) / ( 4 * pi**2 * c_air**2 )
    factor2 = (c_air/c_sea)**2
    factor3 = 3 * g / (twopi*c_sea)
!
    do i=1_int32,oswi%hdr%grid%numberOfPoints
      m=>mask_depth(i)
      if (.not.m) cycle
      z=>depth(i)
      do k=1_int32,oswi%hdr%f%size
        f     => oswi%hdr%f%sound(k)
        kpi   = twopi * f * real(abs(z),double) / c_sea
        call get_ocean_resonance( kpi, R2, A, B, C )
        oswi_mod_coeff_fd(i,k) = factor * f**3 * ( factor1/f**2 + factor2*(A/B) + factor3/f*(C/B) ) 
      end do
    end do
!
! Solid earth
!
  elseif (oswi%hdr%bed) then
!
    factor  = 4 * g**2 * pi**4 * (rho_bed/c_bed)**2
    factor1 = 0._double
    factor2 = (c_bed/c_sea)**2
    factor3 = 3 * g / (twopi * c_sea)
!
    do i=1_int32,oswi%hdr%grid%numberOfPoints
      m=>mask_depth(i)
      if (.not.m) cycle
      z=>depth(i)
      do k=1_int32,oswi%hdr%f%size
        f     => oswi%hdr%f%sound(k)
        kpi   = twopi * f * real(abs(z),double) / c_sea
        call get_ocean_resonance( kpi, T2, A, B, C )
        oswi_mod_coeff_fd(i,k) = factor * f**3 * factor2*(A/B)
      end do
    end do
!
  end if
!
  if (oswi%hdr%debug) oswi%dat%llf%dat = oswi_mod_coeff_fd
!
  return
!
!*****************************************************************************80
End subroutine set_modulation_coefficients_finite_ocean_depth


Subroutine get_ocean_resonance( kpi, R2, A, B, C )
!*****************************************************************************80
!
!! GET_OCEAN_RESONANCE 
!
!  Description:
!
!    Calculate resonance coefficients (Waxler et al.) for the ocean depth. 
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
!    Input, real(double) kpi, angular frequency.
!
!    Input, real(double) R2, reflection coefficient squared. 
!
!    Output, real(double) A, resonance coefficient A.
!
!    Output, real(double) B, resonance coefficient B.
!
!    Output, real(double) C, resonance coefficient C.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  real(double), intent(in)   :: kpi, R2
  real(double), intent(out)  :: A, B, C
!
! Local variables
!
  real(double)  :: bsin, bcos, bsin2, bcos2
!
!  ---
!
  bsin  = dsin( kpi )
  bcos  = dcos( kpi )
  bsin2 = bsin ** 2
  bcos2 = bcos ** 2
!
  A = R2 * bcos2 + bsin2
  B = bcos2 + R2 * bsin2
  C = ( 1._double - R2 ) * bsin * bcos
!
  return
!
!*****************************************************************************80
End subroutine get_ocean_resonance


Subroutine calc_modes ( f, depth, beta, c, modes )
!*****************************************************************************80
!
!! CALC_MODES 
!
!  Description:
!
!    Calculate resonance coefficients for Rayleigh wave coupling (Longuet Higgins
!    and Kedar et al.) 
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
!    Input, real(double) f, frequency in Hz.
!
!    Input, real(double) beta, shear wave velocity in m/s. 
!
!    Input, real(single) depth, bedrock depth below sea level in m. 
!
!    Output, real(double) c, total resonance coefficient.
!
!    Optional input, integer(int32) modes, number of modes to take into account.
!
!
!****************************
!
  implicit none
!
! Dummy variables
!
  real(double)  , intent(in)             :: f, beta
  real(single)  , intent(in)             :: depth
  real(double)  , intent(out)            :: c
  integer(int32), intent(in) , optional  :: modes
!
! Local variables
!
  integer(int32)  :: ix, nmodes
  real(single)    :: x
  real(single), dimension(:), pointer :: cx
!
!  ---
!
  nmodes=nc_n
  if(present(modes)) then
    if(modes.gt.0_int32.and.modes.le.nc_n) nmodes=modes
  endif
  x  = real(f*depth/beta,single) ! c_x is table is in Hz, and f=f_ac !!
  ix = minloc(abs(c_x-x),1)
  cx => c_n(1:nmodes,ix)
  c  = sum(real(cx,kind=double)**2)
!
  return
!
!*****************************************************************************80
End subroutine calc_modes
