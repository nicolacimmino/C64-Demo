100 REM *****************************************************************************
110 REM * Flicker border color.                                                     *
120 REM *                                                                           *
130 REM * Copyright (C) 2016 Nicola Cimmino                                         *
140 REM *                                                                           *
150 REM *   This program is free software: you can redistribute it and/or modify    *
160 REM *   it under the terms of the GNU General Public License as published by    *
170 REM *   the Free Software Foundation, either version 3 of the License, or       *
180 REM *   (at your option) any later version.                                     *
190 REM *                                                                           *
200 REM *  This program is distributed in the hope that it will be useful,          *
210 REM *   but WITHOUT ANY WARRANTY; without even the implied warranty of          *
220 REM *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *
230 REM *   GNU General Public License for more details.                            *
240 REM *                                                                           *
250 REM *   You should have received a copy of the GNU General Public License       *
260 REM *   along with this program.  If not, see http://www.gnu.org/licenses/.     *
270 REM *                                                                           *
280 REM *                                                                           *
290 REM *****************************************************************************
300 REM
310 POKE 53280,(PEEK(53280)+1) AND 15
320 GOTO 310