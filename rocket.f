c     AUTHOR: Hannah Moore
c     DATE WRITTEN: 02/04/2020

      PROGRAM rocket

      double precision alt,a,den,d
      real pitch_angle_val, thrust_val, weight_val, acc_val, pi
      real prev_vel, d_val, vel_val, vertical_val, altitude, den_val, surface_area
      integer count, input_val
      double precision lis(19,2), data(131,6)
      character w*25,x*25,y*25,z*25
      count=0
      pi=3.14159265
     

      open(11, file="airDensities.txt", status='old')

c     READING AND STORING FILE DATA

      do 10 i=1, 22
         count = count + 1
         if (count .LT. 4) then
            read(11,*)
         endif
         if (count .GT. 3) then
            read(11,*)alt,a,den,d
	    lis(i-3,1) = alt
	    lis(i-3,2) = den
         endif
   10 continue

      close(11,status='keep')


      surface_area=1
      vel_val=0
      vertical_val=0
      altitude=0

c     CALCULATING TABLE DATA

      do 40 j=1, 131         


         if(j .LT. 2) then
            acc_val=0
            d_val=0
            prev_vel=0
            weight_val = 7000
         endif


         if (j .GT. 1) then

c           weight_val is decreased at the same rate each second until 120 secs is reached
            weight_val = 3000+(4000-((j-2)*33.3333333))
  
c        loop to calculate density value used for drag function
         do 35 k=1, 19
            alt_val = lis(k,1)
            
            if(k .EQ. 1) then
               den_val = lis(1,2) * (.0001)
            endif

c     compares calculated altitude with altitudes read from text file to determine density
            if(altitude .GT. alt_val) then
               den_val = lis(k+1,2) * (.0001)
            endif

            if(altitude .EQ. alt_val) then
               den_val = lis(k+1,2) * (.0001)
            endif

            if(altitude .GT. 250000) then
               den_val = lis(19,2) * (.0001)
            endif
   35    continue

         acc_val = acceleration(thrust_val, pitch_angle_val, weight_val)
         endif



         pitch_angle_val = (pi/2)
         thrust_val = 1000000


         if (j .GE. 22) then
            pitch_angle_val = (13*pi)/30
         endif

         
c     after stage 1 is over
         if(j .GT. 121) then
            acc_val = -9.80
            weight_val = 3000
            pitch_angle_val = (13*pi)/30

            d_val = drag(den_val, prev_val, surface_area)

            vel_val = velocity(prev_val, d_val, -9.80)

            prev_val = vel_val

            vertical_val = vel_val * sin(pitch_angle_val)
         endif



         d_val = drag(den_val, prev_val, surface_area)

         vel_val = velocity(prev_val, d_val, acc_val)

         prev_val = vel_val

         vertical_val = vel_val * sin(pitch_angle_val)

         altitude = altitude + vertical_val


         data(j,1) = j-1
	 data(j,2) = acc_val
         data(j,3) = d_val
         data(j,4) = vel_val
         data(j,5) = vertical_val
         data(j,6) = altitude     
   
   40 continue

c     FORMATTING/PRINTING TABLE

      write(*, fmt="(aaa)", advance="no")'	 time	',' acceleration	'
      write(*,fmt="(aaa)", advance="no")'   drag	','	   velocity'
      write(*,fmt="(aaa)", advance="no")'	 v. velocity','	   altitude'
      print*,""
      
      do 55 m=1, 105
         write(*, fmt="(aaa)", advance="no")'-'
   55 continue

      do 60 m=1, 131
         print*,""
         do 50 n=1, 6
           if(n .EQ. 1) then
	      write(*, fmt="(a,f4.0)", advance="no") "	",data(m, n)
           endif
           if(n .GT. 1 .AND. n .LT. 4) then
	      write(*, fmt="(a,f8.2)", advance="no") "	",data(m, n)
           endif
           if(n .GT. 3 .AND. n .LT. 7) then
	      write(*, fmt="(a,f12.4)", advance="no") "	",data(m, n)
           endif
   50    continue
   60 continue
      print*,""

      do 65 m=1, 105
         write(*, fmt="(aaa)", advance="no")'-'
   65 continue
      print*,""


c      USER INPUT

      write(*,*)'Enter a time since launch (in seconds): '
      read(*,*)input_val
      
      write(*,*)'At T+',input_val, ' seconds:'
      print*,""


c     FORMATTING

      write(*, fmt="(aaa)", advance="no")'	 time	',' acceleration	'
      write(*,fmt="(aaa)", advance="no")'   drag	','	   velocity'
      write(*,fmt="(aaa)", advance="no")'	 v. velocity','	   altitude'
      print*,""
      
      do 70 m=1, 105
         write(*, fmt="(aaa)", advance="no")'-'
   70 continue
      print*,""



c     PRINTING/FORMATTING USER INPUT

      do 75 k=1, 131
         do 80 l=1, 6
           if(k .EQ. input_val) then
              if(l .EQ. 1) then
	         write(*, fmt="(a,f4.0)", advance="no") "	",data(k+1, l)
              endif
              if(l .GT. 1 .AND. l .LT. 4) then
	         write(*, fmt="(a,f8.2)", advance="no") "	",data(k+1, l)
              endif
              if(l .GT. 3 .AND. l .LT. 7) then
	         write(*, fmt="(a,f12.4)", advance="no") "	",data(k+1, l)
              endif
           endif
   80    continue
   75 continue
      print*,""

      do 90 m=1, 105
         write(*, fmt="(aaa)", advance="no")'-'
   90 continue
      print*,""


      END PROGRAM


c     AUTHOR: Hannah Moore, DATE WRIITEN: 02/02/2020
c     PARAMETERS: thrust = value is always 1,000,000, 
c                 pitch_angle = value varies depending on time,
c                 weight = values decrease each second until last 10 secs then value is 3000
c     RETURN: type = real, description  = value of acceleration for each second
c     PURPOSE: calculates the acceleration value for each second of rocket launch

      real function acceleration(thrust, pitch_angle, weight)
      acceleration = thrust / (weight / sin(pitch_angle))-9.8
      return
      end

c     AUTHOR: Hannah Moore, DATE WRIITEN: 02/03/2020
c     PARAMETERS: density = value is determined by text file data, 
c                 velocity = value is the last velocity calculated
c                 area = value is the surface area affected (stays 1)
c     RETURN: type = real, description  = value of drag for each second
c     PURPOSE: calculates the drag value for each second of rocket launch

      real function drag(density, velocity, area)
      drag = .75 * ((density*(velocity**2)) / 2) * area
      return
      end

c     AUTHOR: Hannah Moore, DATE WRIITEN: 02/03/2020
c     PARAMETERS: prev_velocity = the last velocity calculated, 
c                 drag = the drag calculated for the corresponding second,
c                 acceleration = the acceleration calculated for the corresponding sec
c     RETURN: type = real, description  = value of velocity for each second
c     PURPOSE: calculates the velocity value for each second of rocket launch

      real function velocity(prev_velocity, drag, acceler)
      velocity = (prev_velocity + acceler) - drag
      return
      end

