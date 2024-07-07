' Sonar game

Sub showInstructions()
    Print "Instructions:"
    Print
    Print "You are the captain of the Simon, a treasure-hunting ship. Your current mission"
    Print "is to find the three sunken treasure chests that are lurking in the part of the"
    Print "ocean you are in and collect them."
    Print 
    Print "To play, enter the coordinates of the point in the ocean you wish to drop a"
    Print "sonar device. The sonar can find out how far away the closest chest is to it."
    Print "For example, the d below marks where the device was dropped, and the 2's"
    Print "represent distances of 2 away from the device. The 4's represent"
    Print "distances of 4 away from the device."
    Print 
    Print "    444444444"
    Print "    4       4"
    Print "    4 22222 4"
    Print "    4 2   2 4"
    Print "    4 2 d 2 4"
    Print "    4 2   2 4"
    Print "    4 22222 4"
    Print "    4       4"
    Print "    444444444"
    Print "Press ENTER to continue...";
    
    Input " ", Pausing
    
    Print "For example, here is a treasure chest (the c) located a distance of 2 away from"
    Print "the sonar device (the d):"
    Print 
    Print "22222"
    Print "c   2"
    Print "2 d 2"
    Print "2   2"
    Print "22222"
    Print 
    Print "The point where the device was dropped will be marked with a d."
    Print 
    Print "The treasure chests don't move around. Sonar devices can detect treasure"
    Print "chests up to a distance of 9. If all chests are out of range, the point"
    Print "will be marked with O"
    Print 
    Print "If a device is directly dropped on a treasure chest, you have discovered"
    Print "the location of the chest, and it will be collected. The sonar device will"
    Print "remain there."
    Print 
    Print "When you collect a chest, all sonar devices will update to locate the next"
    Print "closest sunken treasure chest."
    Print "Press ENTER for game beginning.";
    
    Input " ", Pausing
    
End Sub   
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Function getNewLine$()

    Local oneString$ = "", x

    For x = 0 To 59

        If Int((Rnd * 2)) Then
            oneString$ = oneString$ + "~"
        Else
            oneString$ = oneString$ + "`"
        EndIf

    Next
    getNewLine$ = oneString$
End Function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Sub showScreen()

    hline$ = "     "
    
    For i = 1 To 5
        hline$ = hline$ + Space$(9) + Str$(i)
    Next
    
    cls
    Print hline$;Space$(13);"S O N A R ! "
    
    Print "    "+"012345678901234567890123456789012345678901234567890123456789                "
    
    For i = 0 To 14
    
        If i < 10 Then
            extraSpace$ = " "
        Else
            extraSpace$ = ""
        EndIf
        
        If sonarDevices < 10 Then
            extraSsonar$ = " "
        Else
            extraSsonar$ = ""
        EndIf   

        If SonPos_x < 10 Then
            extraSonPos_x$ = " "
        Else
            extraSonPos_x$ = ""
        EndIf

        If SonPos_y < 10 Then
            extraSonPos_y$ = " "
        Else
            extraSonPos_y$ = ""
        EndIf  
        
        If i = 1 Then
            Print extraSpace$;i;" ";theBoard$(i);i;"    Sonars    "
        ElseIf i = 2 Then
            Print extraSpace$;i;" ";theBoard$(i);i;"     ";extraSsonar$;sonarDevices;"      "            
        ElseIf i = 4 Then
            Print extraSpace$;i;" ";theBoard$(i);i;"    Chests    "
        ElseIf i = 5 Then
            Print extraSpace$;i;" ";theBoard$(i);i;"      ";numChests;"      " 
        ElseIf i = 10 Then
            Print extraSpace$;i;" ";theBoard$(i);i;"  Last Move "
        ElseIf i = 11 Then
            Print extraSpace$;i;" ";theBoard$(i);i;"  ";extraSonPos_x$;SonPos_x;"  ";extraSonPos_y$;SonPos_y;" " 
        Else
            Print extraSpace$;i;" ";theBoard$(i);i;extraSpace$;"             "       
        EndIf
    
    Next
    
    Print "    "+"012345678901234567890123456789012345678901234567890123456789                "
    
    Print hline$
    Print Space$(79)
    Print Space$(79)
   
    Print @(0, 216) " "

End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Sub upd_Board(p_UBD_x, p_UBD_y, p_Str$)

    theBoard$(p_UBD_y) = LEFT$(theBoard$(p_UBD_y),p_UBD_x) + p_Str$ + RIGHT$(theBoard$(p_UBD_y), 59 - p_UBD_x)

End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Function IsValidMove( p_x, p_y )
    If (p_x >= 0 And p_x <= 59) And (p_y >= 0 And p_y <= 14) Then
        IsValidMove = 1
     Else
        IsValidMove = 0
     EndIf
End Function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Dim theBoard$(14) length 60
Dim previousMoves(15, 2)
Dim chests(2, 2)
Dim SonPos_x, SonPos_y 

endnumChests = 2 

cls
Print

Print "  -- S O N A R -- game"
   
Print
Input "  '1' for instructions: ", Pausing

If Pausing = 1 Then
    cls
    showInstructions()
    cls
EndIf    

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

gameBEGIN:

sonarDevices = 16
numChests = 3
counterMoves = 0

For i = 0 To numChests - 1

    chests(i,0) = Int((Rnd * 60))
    chests(i,1) = Int((Rnd * 15))
    'chest activity
    chests(i,2) = 1         
    
Next

'Print " Chests: ("chests(0,0)","chests(0,1)","chests(0,2);" ); ("chests(1,0);","chests(1,1)","chests(1,2);" ); ("chests(2,0);","chests(2,1)","chests(2,2);" )"

For iter = 0 To 14
    theBoard$(iter) = getNewLine$()
Next

For i_Mov = 0 To sonarDevices - 1

    previousMoves(i, 0) = 0
    previousMoves(i, 1) = 0 
    previousMoves(i, 2) = 0

Next
 
sonarsLoop:

DO While sonarDevices > 0

    showScreen()
    
    If numChests = 0 Then
        print " You have found all treasure chests! Congratulations and gg!"
        goto theEND 
    EndIf
   
    Print " Where do you want to drop the next sonar device (0-59, 0-14)";
    Input SonPos_x, SonPos_y

    If IsValidMove( SonPos_x, SonPos_y ) = 0 Then
        Print " Need num from 0 to 59 'comma' then from 0 to 14. ";
        Input "Press ENTER", Pausing
        goto sonarsLoop
    Endif
    
    'we must track all moves so that sonar devices can be updated
    previousMoves(counterMoves, 0) = SonPos_x
    previousMoves(counterMoves, 1) = SonPos_y 
    previousMoves(counterMoves, 2) = 1
    
    counterMoves = counterMoves + 1    
    i_Moves = 0
    updAllSonars = 0    
    
    FOR_UPD_Sonars:
         
    smallestDistance = 100

    For i_Dist = 0 To endnumChests

        If chests(i_Dist,2) = 1 Then
        
            cx = chests(i_Dist, 0)
            cy = chests(i_Dist, 1)

            If Abs(cx - SonPos_x) > Abs(cy-SonPos_y) Then
                distance = Abs(cx - SonPos_x )
            Else
                distance = Abs(cy - SonPos_y)
            EndIf

            If distance < smallestDistance Then
                smallestDistance = distance
            EndIf
        EndIf
    Next

    If smallestDistance = 0 Then
    
        For i_Min = 0 To endnumChests
        
            If chests(i_Min, 2) = 1 Then        

                If chests(i_Min, 0) = SonPos_x And chests(i_Min, 1) = SonPos_y Then
    
                    Print " You have found a sunken treasure chest! ";
                    Input "Press ENTER", Pausing
                    updAllSonars = 1
    
                    'deactivate chest
                    chests(i_Min,2) = 0
                    numChests = numChests - 1

                    goto forOUT
    
                EndIf
            EndIf
        Next
        
    Else

        If smallestDistance < 10 Then
            upd_Board(SonPos_x, SonPos_y, Str$(smallestDistance))
            'Print " Treasure detected at a distance of "+ Str$(smallestDistance) + " from the sonar device."
        Else
            upd_Board(SonPos_x, SonPos_y, "O")
            'Print " Sonar did not detect anything. All treasure chests out of range."
        EndIf

    EndIf
    
    forOUT:    
     
    If updAllSonars = 1 Then
    
        ' update all the sonar devices currently on the map.

        onceMore:
        
        If i_Moves > (counterMoves - 1) Then
            goto upd_OUT
        Else
            If previousMoves(i_Moves, 2) = 1 Then
                SonPos_x = previousMoves(i_Moves, 0)
                SonPos_y = previousMoves(i_Moves, 1)

                i_Moves = i_Moves + 1
                goto FOR_UPD_Sonars                
            Else
                i_Moves = i_Moves + 1
                goto onceMore
            EndIf          
        
        EndIf         
    
    EndIf

    upd_OUT:   
    sonarDevices = sonarDevices - 1

LOOP

If sonarDevices <= 0 Then

    Print @(0, 216) " "
    Print "  Run out of sonar devices! Chests were here: (";chests(0,0);",";chests(0,1);" ) ("chests(1,0);",";chests(1,1);" ) (";chests(2,0);",";chests(2,1);" )"   

EndIf

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

theEND:

Input "  '9' for quit, ENTER for continue: ", Pausing

If Pausing <> 9 Then
    goto gameBEGIN
EndIf    

End
