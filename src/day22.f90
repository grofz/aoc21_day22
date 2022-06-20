  module day22
    implicit none
    private
    public box_t, handle_t
    integer, parameter, public :: I8 = selected_int_kind(18)

    type box_t
      private
      integer :: x(2,3)
      logical :: neg = .false.
    contains
      procedure :: getneg => box_getneg
      procedure :: flipneg => box_flipneg
      procedure :: volu => box_volume
      procedure, private :: box_intersection
      generic :: operator(*) => box_intersection
      procedure :: print => box_print
    end type

    interface box_t
      module procedure box_new
    end interface 

    type handle_t
      private
      logical :: eof = .true.
      integer :: fid = 0
    contains
      procedure :: iseof => handle_iseof
      procedure :: open => handle_open
      procedure :: get => handle_get
    end type
  contains

    function box_new(xr,yr,zr,neg)
      integer, intent(in) :: xr(2), yr(2), zr(2)
      logical, optional :: neg
      type(box_t) :: box_new

      box_new % x(:,1) = xr
      box_new % x(:,2) = yr
      box_new % x(:,3) = zr
      if (present(neg)) box_new % neg = neg
    end function


    pure logical function box_getneg(this)
      class(box_t), intent(in) :: this
      box_getneg = this % neg
    end function


    subroutine box_flipneg(this)
      class(box_t), intent(inout) :: this
      this % neg = .not. this % neg
    end subroutine


    pure function box_volume(this) result(volu)
      class(box_t), intent(in) :: this
      integer(kind=I8) :: volu
      integer :: j
      volu = 1_I8
      do j=1,3
        volu = volu * int(max(0, this%x(2,j)-this%x(1,j)+1), kind=I8)
      end do
      if (this % neg) volu = -volu
    end function


    function box_intersection(a, b) result(aub)
      class(box_t), intent(in) :: a, b
      type(box_t) :: aub

      integer :: j
      do j=1,3
        aub % x(:,j) = onedim_intersection(a % x(:,j), b % x(:,j))
      end do
      aub % neg = xor(a % neg, b % neg)
    end function

    function onedim_intersection(xa, xb) result(xi)
      integer, intent(in) :: xa(2), xb(2)
      integer :: xi(2)

      integer :: a(2), b(2)

      if (xa(1) <= xb(1)) then
        a = xa
        b = xb
      else
        a = xb
        b = xa
      end if

      if (b(1) > a(2)) then
        xi(1) = 0
        xi(2) = -1
        !xi = [0, -1]
      else
        xi(1) = max(a(1), b(1))
        xi(2) = min(a(2), b(2))
        !xi = [max(a(1), b(1)), min(a(2), b(2))]
      endif
    end function



    function handle_iseof(this)
      class(handle_t), intent(in) :: this
      logical :: handle_iseof
      handle_iseof = this % eof
    end function


    subroutine handle_open(this, file)
      class(handle_t), intent(out) :: this
      character(len=*), intent(in) :: file
      character(len=5000) :: line
      integer :: ios
      open(newunit=this%fid, file=file, status='old')
      read(this%fid,'(a)',iostat=ios) line
      if (ios == 0) then
        backspace(this%fid)
        this % eof = .false.
      else
        this % eof = .true.
        close(this%fid)
        this % fid = 0
      end if
    end subroutine


    function handle_get(this) result(box)
      class(handle_t), intent(inout) :: this
      type(box_t) :: box

      character(len=5000) line
      character(len=3) word
      integer :: ios, b1
      if (this%fid==0 .or. this%eof) error stop 'handle_get - reading error'
      read(this%fid,'(a)') line

      ! process the "line"
      b1 = scan(line,' ')
      read(line(:b1-1),'(a)') word
      line = line(b1+1:)
      select case (word)
      case('on')
        box % neg = .false.
      case('off')
        box % neg = .true.
      case default
        error stop 'handle_get - format error'
      end select
      call getnumber(line, box%x(1,1))
      call getnumber(line, box%x(2,1))
      call getnumber(line, box%x(1,2))
      call getnumber(line, box%x(2,2))
      call getnumber(line, box%x(1,3))
      call getnumber(line, box%x(2,3))
 call box % print()

      ! test if was the final line
      read(this%fid,'(a)',iostat=ios) line
      if (ios == 0) then
        backspace(this%fid)
      else
        this % eof = .true.
        close(this%fid)
        this % fid = 0
      end if
    contains
      subroutine getnumber(line,num)
        character(len=*), intent(inout) :: line
        integer, intent(out) :: num
        integer :: a1, a2

        a1 = scan(line,'-0123456789')
        a2 = scan(line(a1:),'.,')
        if (a2==0) then
            a2=len(trim(line))
        else
            a2=(a1-1)+a2-1
        endif
        read(line(a1:a2),*) num
        line = line(a2+1:)
      end subroutine
    end function


    subroutine box_print(this)
       class(box_t), intent(in) :: this
       integer(I8) :: volu
       print '(l1,1x,3("[",i0,":",i0,"]"),"  V=",i0)',.not. this%neg, this%x, this%volu()
    end subroutine

  end module day22
