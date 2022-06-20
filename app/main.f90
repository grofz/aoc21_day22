program main
  use day22
  implicit none

  type(handle_t) :: fid
  type(box_t), allocatable :: obj(:)
  type(box_t) :: new, isect, part_one_clip
  integer :: i
  integer(I8) :: totvol
! logical, parameter :: IS_PART_ONE = .true.
  logical, parameter :: IS_PART_ONE = .false.

  call fid % open('input.txt')
! call fid % open('sample.txt')
! call fid % open('small_sample.txt')

  part_one_clip = box_t([-50,50],[-50,50],[-50,50])

  allocate(obj(0))
  totvol = 0
  do
    if (fid % iseof()) exit
    new = fid % get()

    if (IS_PART_ONE) then
      new = new*part_one_clip
      if (new % volu() == 0) cycle
    end if

    if (.not. new % getneg()) then ! "new" is ON
      ! add box at the end of the list
      obj = [obj, new]
      totvol = totvol + new % volu()

      ! add intersections of box with all previous boxes as MINUS
      do i=1, size(obj)-1
        isect = obj(i)*new
        if (isect%volu()==0) cycle
        call isect % flipneg()
        obj = [obj, isect] 
        totvol = totvol + isect % volu()
      end do

    else ! "new" is OFF
      ! add intersections of box with all previous boxes as PLUS
      do i=1, size(obj)
        isect = obj(i)*new
        if (isect%volu()==0) cycle
        obj = [obj, isect]
        totvol = totvol + isect % volu()
      end do
    end if

    print '("Volume so far = ",i0)', totvol
    print *
  end do


  print *, 'List size =',size(obj)
  print *, 'Answer is ',totvol, 1285677377848549_I8==totvol

end program main



