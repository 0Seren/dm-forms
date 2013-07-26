dm-Forms
========

An alternative way to create forms using the Play (Scala) framework.

This is basically a port of the forms module in Django, ported to Scala. (The Django project
and its contributors do *not* endorse this project--we thank them for the inspriration and the
idea for the architecture, but any mistakes are our own.)

Its biggest contribution, in our opinion, is that `Form` instances know how to render themselves
using Twitter Bootstrap, so they look pretty. Validation is easy, and occurs both client- and
server-side, and `Field`s give you back Scala instances that have already been converted into the
type you were expecting.

For more information visit http://dupontmanual.github.io/dm-forms/.

Released under the Apache 2 License, to be completely compatible with Play licensing.

All code in this project is copyright 2013 by Allen Boss and Todd O'Bryan.