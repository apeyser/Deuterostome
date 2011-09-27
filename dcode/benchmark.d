| Copyright 2011 Alexander Peyser & Wolfgang Nonner
|
| This file is part of Deuterostome.
|
| This program is free software: you can redistribute it and/or modify
| it under the terms of the GNU General Public License as published by
| the Free Software Foundation, either version 2 of the License, or
| (at your option) any later version.
|
| This program is distributed in the hope that it will be useful,
| but WITHOUT ANY WARRANTY; without even the implied warranty of
| MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
| GNU General Public License for more details.
|
| You should have received a copy of the GNU General Public License
| along with this program.  If not, see <http://www.gnu.org/licenses/>.
/test {
  (Integer test ) toconsole
  gettime 0.0 0.0 1.0 1e8 { add } for pop gettime exch sub _ pop
  /x 10000 /d array def /y 10000 /d array def
  (Float test ) toconsole
  0.0 y copy pop 1.123123212 x copy pop
  gettime y 1000000 { x add } repeat pop gettime exch sub _ pop
  (Matrix test ) toconsole
  /x 1e6 /d array def
  gettime 1000 { 17.4 x copy 19.5 mul pop } repeat gettime exch sub _ pop
} bind def
