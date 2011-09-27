| -*- mode: d; -*-
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

|============================ D Startup =================================

|============================= userdict =================================

/dm_type /dgen def

save /startup_common_save name 
/startup_common_buf vmstatus sub 10 div /b array def
startup_common_save capsave {
  getstartupdir (startup_common.d) startup_common_buf readfile mkact exec
  getstartupdir (startup_libs.d) startup_common_buf readfile mkact exec
} stopped startup_common_save restore {
  1024 /b array 0 
  (Unable to load: ) fax
  getstartupdir fax (startup_common.d\n) fax
  0 exch getinterval toconsole
  stop
} if

| (source) code
/error_ops_length 2 def

| -- | false
/_makeerror {false} def

|============================= userdict =================================

/lock   ~exec bind def    | just to make dnodes & dvts symmetric
/unlock ~exec bind def


(dgen) userdict /myid put

(End of startup\n) toconsole
