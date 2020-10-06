Module string_utility

   implicit none 

   character( * ), private, parameter :: lower_case = 'abcdefghijklmnopqrstuvwxyz' 
   character( * ), private, parameter :: upper_case = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' 

Contains 

! =========================================================================================
!
!.......function StrUpCase
!
!	Written by:  
!
!       Function to change a string to upper case.
!         
!	INPUT:
!
!	input_string  :  Input string
!
!	OUTPUT:
!
!	output_string :  Output string
!
!****************************
   Function strupcase ( input_string ) result ( output_string ) 
!****************************
!
!.....Dummy variables
!
      character ( len = * ), intent ( in ) :: input_string 
      character ( len ( input_string ) ) :: output_string
!
!.....Local variables
!
      integer :: i, n
!
!  ---
!
!.....Copy input string
!
      output_string = input_string
!
!.....Loop over string elements
!
      do i = 1, len ( output_string )
!
!........Find location of letter in lower case constant string
!
         n = index ( lower_case, output_string( i : i ) ) 
!
!........ If current substring is a lower case letter, make it upper case
!
         if ( n .ne. 0 ) output_string( i:i ) = upper_case( n : n )
!
      end do 
!
      return
!
   End function strupcase 
!
! =========================================================================================


! =========================================================================================
!
!.......subroutine upper
!
!
!       Function to change a string to upper case.
!         
!	INOUT:
!
!	str  :  character string
!
!
!****************************
   Subroutine upper ( str ) 
!****************************
!
!.....Dummy variables
!
      character( len = * ), intent( inout ) :: str
!
!.....Local variables
!
      integer :: i, n 
!
!  ---
!
!....Loop over string elements 
!
      do i = 1, len ( str ) 
!
!.........Find location of letter in upper case constant string
!
         n = index ( lower_case, str( i : i ) )
!
!......If current substring is an upper case letter, make it lower case
!
         if ( n .ne. 0 ) str( i : i ) = upper_case( n : n ) 
!
      end do
!
      return
!
   End subroutine upper
!
! =========================================================================================


! =========================================================================================
!
!.......function StrLowCase
!
!	Written by:  
!
!       Function to change a string to lower case.
!         
!	INPUT:
!
!	input_string  :  Input string
!
!	OUTPUT:
!
!	output_string :  Output string
!
!****************************
   Function strlowcase ( input_string ) result ( output_string ) 
!****************************
!
!.....Dummy variables
!
      character( len = * ), intent ( in ) :: input_string 
      character( len ( input_string ) ) :: output_string 
!
!.....Local variables
!
      Integer :: i, n 
!
!  ---
!
!.....Copy input string
!
      Output_String = Input_String
!
!....Loop over string elements 
!
      do i = 1, len ( output_string ) 
!
!.........Find location of letter in upper case constant string
!
         n = index ( upper_case, output_string( i : i ) )
!
!......If current substring is an upper case letter, make it lower case
!
         if ( n .ne. 0 ) output_string( i : i ) = lower_case( n : n ) 
!
      end do
!
      return
!
   End function strlowcase 
!
! =========================================================================================


! =========================================================================================
!
!.......subroutine lower
!
!
!       Function to change a string to lower case.
!         
!	INOUT:
!
!	str  :  character string
!
!
!****************************
   Subroutine lower ( str ) 
!****************************
!
!.....Dummy variables
!
      character( len = * ), intent( inout ) :: str
!
!.....Local variables
!
      Integer :: i, n 
!
!  ---
!
!....Loop over string elements 
!
      do i = 1, len ( str ) 
!
!.........Find location of letter in upper case constant string
!
         n = index ( upper_case, str( i : i ) )
!
!......If current substring is an upper case letter, make it lower case
!
         if ( n .ne. 0 ) str( i : i ) = lower_case( n : n ) 
!
      end do
!
      return
!
   End subroutine lower
!
! =========================================================================================


! =========================================================================================
!
!.......function strcompress
!
! Remove white spaces from a string
!
!****************************
   Function strcompress ( input_string ) result ( output_string ) 
!****************************
!
!.....Dummy variables
!
      character ( len=* ), intent ( in ) :: input_string 
      character ( len(input_string) )    :: output_string
!
!.....Local variables
!
      integer :: i, n, ia
!
!  ---
!
!.....Copy input string
!
      output_string = '' 
!
!.....Loop over string elements
!
      n=0
      do i=1, len( input_string )
         ia=iachar( input_string(i:i) )
         if ( ia.ne.32 .and. ia.ne.9 ) then
            n=n+1
            output_string(n:n)=input_string(i:i)
         end if
      end do 
!
      return
!
   End function strcompress 
!
! =========================================================================================


! =========================================================================================
!
!.......function strcompresstab
!
! Remove white spaces from a string
!
!****************************
   Function strcompresstab ( input_string ) result ( output_string ) 
!****************************
!
!.....Dummy variables
!
      character ( len=* ), intent ( in ) :: input_string 
      character ( len(input_string) )    :: output_string
!
!.....Local variables
!
      integer :: i, n, ia
!
!  ---
!
!.....Copy input string
!
      output_string = '' 
!
!.....Loop over string elements
!
      n=0
      do i=1, len( input_string )
         ia=iachar( input_string(i:i))
         if ( ia.ne.9 ) then
            n=n+1
            output_string(n:n)=input_string(i:i)
         end if
      end do 
!
      return
!
   End function strcompresstab 
!
! =========================================================================================


! =========================================================================================
!
!.......function strcompressedges
!
! Remove white spaces from a string
!
!****************************
   Function strcompressedges ( input_string ) result ( output_string ) 
!****************************
!
!.....Dummy variables
!
      character ( len=* ), intent ( in ) :: input_string 
      character ( len(input_string) )    :: output_string
!
!.....Local variables
!
!  ---
!
!.....Remove tabs and trim left
!
      output_string = trim(adjustl(strcompresstab(input_string)))
!
      return
!
   End function strcompressedges
!
! =========================================================================================


End module string_utility
