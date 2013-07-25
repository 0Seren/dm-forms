dm-forms
========

An alternative way to create forms using the Play (Scala) framework.

This is basically a port of the forms module in Django, ported to Scala. Its biggest contribution,
in our opinion, is that `Form` instances know how to render themselves using Twitter Bootstrap, so
they look pretty. Validation is easy, and occurs both client- and server-side, and `Field`s give
you back Scala instances that have already been converted into the type you were expecting.
