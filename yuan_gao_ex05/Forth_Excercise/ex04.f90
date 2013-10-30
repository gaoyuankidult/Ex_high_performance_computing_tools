program test_read
  implicit none
  character :: buffer
  integer :: dist_array(10)
  integer::sequence_number,longest_sequence
  integer::ivalue,old_ivalue,long_seq_value
  integer::io
  dist_array = 0
  sequence_number = 0
  longest_sequence = 0
  ivalue = 0
  old_ivalue = 0
  long_seq_value = 0
  !open (unit=15, file="pi4e6.dat")  
  open (unit=15, file="test.dat")
  do while(1 == 1)
     read (15,'(A1)',advance='NO', iostat = io),buffer
     ivalue = ichar(buffer) -48
     if(ivalue == old_ivalue) then
        sequence_number=sequence_number +1
        if(sequence_number > longest_sequence) then
           longest_sequence=sequence_number
           long_seq_value = ivalue
        end if
     else
        sequence_number = 1
     end if
     old_ivalue = ivalue

     if(ivalue >= 0 .and. ivalue < 10) then
        dist_array(ivalue+1)=dist_array(ivalue+1) +1
     end if
     if (io .eq. -1) then
        write(*,*)  "EOF encountered"
        exit
     end if
  end do
  close(15)

  print *, "The distribution trough 0 to 9 :"
  print *, dist_array
  print *, "Number with the longest sequence :"
  print *, long_seq_value
  print *, "The length of longest sequence :"
  print *, longest_sequence
end program test_read
