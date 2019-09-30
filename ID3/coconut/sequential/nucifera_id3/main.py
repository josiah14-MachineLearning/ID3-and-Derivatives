#!/usr/bin/env python
# -*- coding: utf-8 -*-
# __coconut_hash__ = 0xa28fe6e2

# Compiled with Coconut version 1.4.1 [Ernest Scribbler]

# Coconut Header: -------------------------------------------------------------

from __future__ import print_function, absolute_import, unicode_literals, division
import sys as _coconut_sys
if _coconut_sys.version_info < (3,):
    from __builtin__ import chr, filter, hex, input, int, map, object, oct, open, print, range, str, zip, filter, reversed, enumerate, raw_input, xrange
    py_chr, py_hex, py_input, py_int, py_map, py_object, py_oct, py_open, py_print, py_range, py_str, py_zip, py_filter, py_reversed, py_enumerate, py_raw_input, py_xrange, py_repr = chr, hex, input, int, map, object, oct, open, print, range, str, zip, filter, reversed, enumerate, raw_input, xrange, repr
    _coconut_NotImplemented, _coconut_raw_input, _coconut_xrange, _coconut_int, _coconut_long, _coconut_print, _coconut_str, _coconut_unicode, _coconut_repr = NotImplemented, raw_input, xrange, int, long, print, str, unicode, repr
    from future_builtins import *
    chr, str = unichr, unicode
    from io import open
    class object(object):
        __slots__ = ()
        def __ne__(self, other):
            eq = self == other
            if eq is _coconut_NotImplemented:
                return eq
            return not eq
    class int(_coconut_int):
        __slots__ = ()
        if hasattr(_coconut_int, "__doc__"):
            __doc__ = _coconut_int.__doc__
        class __metaclass__(type):
            def __instancecheck__(cls, inst):
                return _coconut.isinstance(inst, (_coconut_int, _coconut_long))
            def __subclasscheck__(cls, subcls):
                return _coconut.issubclass(subcls, (_coconut_int, _coconut_long))
    class range(object):
        __slots__ = ("_xrange",)
        if hasattr(_coconut_xrange, "__doc__"):
            __doc__ = _coconut_xrange.__doc__
        def __init__(self, *args):
            self._xrange = _coconut_xrange(*args)
        def __iter__(self):
            return _coconut.iter(self._xrange)
        def __reversed__(self):
            return _coconut.reversed(self._xrange)
        def __len__(self):
            return _coconut.len(self._xrange)
        def __contains__(self, elem):
            return elem in self._xrange
        def __getitem__(self, index):
            if _coconut.isinstance(index, _coconut.slice):
                args = _coconut.slice(*self._args)
                start, stop, step, ind_step = (args.start if args.start is not None else 0), args.stop, (args.step if args.step is not None else 1), (index.step if index.step is not None else 1)
                return self.__class__((start if ind_step >= 0 else stop - step) if index.start is None else start + step * index.start if index.start >= 0 else stop + step * index.start, (stop if ind_step >= 0 else start - step) if index.stop is None else start + step * index.stop if index.stop >= 0 else stop + step * index.stop, step if index.step is None else step * index.step)
            else:
                return self._xrange[index]
        def count(self, elem):
            """Count the number of times elem appears in the range."""
            return _coconut_int(elem in self._xrange)
        def index(self, elem):
            """Find the index of elem in the range."""
            if elem not in self._xrange: raise _coconut.ValueError(_coconut.repr(elem) + " is not in range")
            start, _, step = self._xrange.__reduce_ex__(2)[1]
            return (elem - start) // step
        def __repr__(self):
            return _coconut.repr(self._xrange)[1:]
        @property
        def _args(self):
            return self._xrange.__reduce__()[1]
        def __reduce_ex__(self, protocol):
            return (self.__class__, self._xrange.__reduce_ex__(protocol)[1])
        def __reduce__(self):
            return self.__reduce_ex__(_coconut.pickle.DEFAULT_PROTOCOL)
        def __hash__(self):
            return _coconut.hash(self._args)
        def __copy__(self):
            return self.__class__(*self._args)
        def __eq__(self, other):
            return _coconut.isinstance(other, self.__class__) and self._args == other._args
    from collections import Sequence as _coconut_Sequence
    _coconut_Sequence.register(range)
    from functools import wraps as _coconut_wraps
    @_coconut_wraps(_coconut_print)
    def print(*args, **kwargs):
        file = kwargs.get("file", _coconut_sys.stdout)
        flush = kwargs.get("flush", False)
        if "flush" in kwargs:
            del kwargs["flush"]
        if _coconut.hasattr(file, "encoding") and file.encoding is not None:
            _coconut_print(*(_coconut_unicode(x).encode(file.encoding) for x in args), **kwargs)
        else:
            _coconut_print(*(_coconut_unicode(x).encode() for x in args), **kwargs)
        if flush:
            file.flush()
    @_coconut_wraps(_coconut_raw_input)
    def input(*args, **kwargs):
        if _coconut.hasattr(_coconut_sys.stdout, "encoding") and _coconut_sys.stdout.encoding is not None:
            return _coconut_raw_input(*args, **kwargs).decode(_coconut_sys.stdout.encoding)
        return _coconut_raw_input(*args, **kwargs).decode()
    @_coconut_wraps(_coconut_repr)
    def repr(obj):
        if isinstance(obj, _coconut_unicode):
            return _coconut_unicode(_coconut_repr(obj)[1:])
        if isinstance(obj, _coconut_str):
            return "b" + _coconut_unicode(_coconut_repr(obj))
        return _coconut_unicode(_coconut_repr(obj))
    ascii = repr
    def raw_input(*args):
        """Coconut uses Python 3 "input" instead of Python 2 "raw_input"."""
        raise _coconut.NameError('Coconut uses Python 3 "input" instead of Python 2 "raw_input"')
    def xrange(*args):
        """Coconut uses Python 3 "range" instead of Python 2 "xrange"."""
        raise _coconut.NameError('Coconut uses Python 3 "range" instead of Python 2 "xrange"')
    if _coconut_sys.version_info < (2, 7):
        import functools as _coconut_functools, copy_reg as _coconut_copy_reg
        def _coconut_new_partial(func, args, keywords):
            return _coconut_functools.partial(func, *(args if args is not None else ()), **(keywords if keywords is not None else {}))
        _coconut_copy_reg.constructor(_coconut_new_partial)
        def _coconut_reduce_partial(self):
            return (_coconut_new_partial, (self.func, self.args, self.keywords))
        _coconut_copy_reg.pickle(_coconut_functools.partial, _coconut_reduce_partial)
else:
    from builtins import chr, filter, hex, input, int, map, object, oct, open, print, range, str, zip, filter, reversed, enumerate
    py_chr, py_hex, py_input, py_int, py_map, py_object, py_oct, py_open, py_print, py_range, py_str, py_zip, py_filter, py_reversed, py_enumerate, py_repr = chr, hex, input, int, map, object, oct, open, print, range, str, zip, filter, reversed, enumerate, repr
    _coconut_str = str
class _coconut(object):
    import collections, copy, functools, types, itertools, operator, threading, weakref, os
    if _coconut_sys.version_info < (3, 2):
        try:
            from backports.functools_lru_cache import lru_cache
            functools.lru_cache = lru_cache
        except ImportError: pass
    if _coconut_sys.version_info < (3,):
        import cPickle as pickle
    else:
        import pickle
    if _coconut_sys.version_info >= (2, 7):
        OrderedDict = collections.OrderedDict
    else:
        OrderedDict = dict
    if _coconut_sys.version_info < (3, 3):
        abc = collections
    else:
        import collections.abc as abc
    class typing(object):
        @staticmethod
        def NamedTuple(name, fields):
            return _coconut.collections.namedtuple(name, [x for x, t in fields])
    Ellipsis, Exception, AttributeError, ImportError, IndexError, KeyError, NameError, TypeError, ValueError, StopIteration, classmethod, dict, enumerate, filter, float, frozenset, getattr, hasattr, hash, id, int, isinstance, issubclass, iter, len, list, locals, map, min, max, next, object, property, range, reversed, set, slice, str, sum, super, tuple, type, zip, repr, bytearray = Ellipsis, Exception, AttributeError, ImportError, IndexError, KeyError, NameError, TypeError, ValueError, StopIteration, classmethod, dict, enumerate, filter, float, frozenset, getattr, hasattr, hash, id, int, isinstance, issubclass, iter, len, list, locals, map, min, max, next, object, property, range, reversed, set, slice, str, sum, super, tuple, type, zip, staticmethod(repr), bytearray
_coconut_sentinel = _coconut.object()
class MatchError(Exception):
    """Pattern-matching error. Has attributes .pattern and .value."""
    __slots__ = ("pattern", "value")
class _coconut_tail_call(object):
    __slots__ = ("func", "args", "kwargs")
    def __init__(self, func, *args, **kwargs):
        self.func, self.args, self.kwargs = func, args, kwargs
_coconut_tco_func_dict = {}
def _coconut_tco(func):
    @_coconut.functools.wraps(func)
    def tail_call_optimized_func(*args, **kwargs):
        call_func = func
        while True:
            wkref = _coconut_tco_func_dict.get(_coconut.id(call_func))
            if (wkref is not None and wkref() is call_func) or _coconut.isinstance(call_func, _coconut_base_pattern_func):
                call_func = call_func._coconut_tco_func
            result = call_func(*args, **kwargs)  # pass --no-tco to clean up your traceback
            if not isinstance(result, _coconut_tail_call):
                return result
            call_func, args, kwargs = result.func, result.args, result.kwargs
    tail_call_optimized_func._coconut_tco_func = func
    tail_call_optimized_func.__module__ = _coconut.getattr(func, "__module__", None)
    tail_call_optimized_func.__name__ = _coconut.getattr(func, "__name__", "<coconut tco function (pass --no-tco to remove)>")
    tail_call_optimized_func.__qualname__ = _coconut.getattr(func, "__qualname__", tail_call_optimized_func.__name__)
    _coconut_tco_func_dict[_coconut.id(tail_call_optimized_func)] = _coconut.weakref.ref(tail_call_optimized_func)
    return tail_call_optimized_func
def _coconut_igetitem(iterable, index):
    if isinstance(iterable, (_coconut_reversed, _coconut_map, _coconut.zip, _coconut_enumerate, _coconut_count, _coconut.abc.Sequence)):
        return iterable[index]
    if not _coconut.isinstance(index, _coconut.slice):
        if index < 0:
            return _coconut.collections.deque(iterable, maxlen=-index)[0]
        return _coconut.next(_coconut.itertools.islice(iterable, index, index + 1))
    if index.start is not None and index.start < 0 and (index.stop is None or index.stop < 0) and index.step is None:
        queue = _coconut.collections.deque(iterable, maxlen=-index.start)
        if index.stop is not None:
            queue = _coconut.list(queue)[:index.stop - index.start]
        return queue
    if (index.start is not None and index.start < 0) or (index.stop is not None and index.stop < 0) or (index.step is not None and index.step < 0):
        return _coconut.list(iterable)[index]
    return _coconut.itertools.islice(iterable, index.start, index.stop, index.step)
class _coconut_base_compose(object):
    __slots__ = ("func", "funcstars")
    def __init__(self, func, *funcstars):
        self.func = func
        self.funcstars = []
        for f, stars in funcstars:
            if _coconut.isinstance(f, _coconut_base_compose):
                self.funcstars.append((f.func, stars))
                self.funcstars += f.funcstars
            else:
                self.funcstars.append((f, stars))
    def __call__(self, *args, **kwargs):
        arg = self.func(*args, **kwargs)
        for f, stars in self.funcstars:
            if stars == 0:
                arg = f(arg)
            elif stars == 1:
                arg = f(*arg)
            elif stars == 2:
                arg = f(**arg)
            else:
                raise _coconut.ValueError("invalid arguments to " + _coconut.repr(self))
        return arg
    def __repr__(self):
        return _coconut.repr(self.func) + " " + " ".join(("..*> " if star == 1 else "..**>" if star == 2 else "..> ") + _coconut.repr(f) for f, star in self.funcstars)
    def __reduce__(self):
        return (self.__class__, (self.func,) + _coconut.tuple(self.funcstars))
    def __get__(self, obj, objtype=None):
        return _coconut.functools.partial(self, obj)
def _coconut_forward_compose(func, *funcs): return _coconut_base_compose(func, *((f, 0) for f in funcs))
def _coconut_back_compose(*funcs): return _coconut_forward_compose(*_coconut.reversed(funcs))
def _coconut_forward_star_compose(func, *funcs): return _coconut_base_compose(func, *((f, 1) for f in funcs))
def _coconut_back_star_compose(*funcs): return _coconut_forward_star_compose(*_coconut.reversed(funcs))
def _coconut_forward_dubstar_compose(func, *funcs): return _coconut_base_compose(func, *((f, 2) for f in funcs))
def _coconut_back_dubstar_compose(*funcs): return _coconut_forward_dubstar_compose(*_coconut.reversed(funcs))
def _coconut_pipe(x, f): return f(x)
def _coconut_star_pipe(xs, f): return f(*xs)
def _coconut_dubstar_pipe(kws, f): return f(**kws)
def _coconut_back_pipe(f, x): return f(x)
def _coconut_back_star_pipe(f, xs): return f(*xs)
def _coconut_back_dubstar_pipe(f, kws): return f(**kws)
def _coconut_assert(cond, msg=None): assert cond, msg if msg is not None else "(assert) got falsey value " + _coconut.repr(cond)
def _coconut_bool_and(a, b): return a and b
def _coconut_bool_or(a, b): return a or b
def _coconut_none_coalesce(a, b): return a if a is not None else b
def _coconut_minus(a, *rest):
    if not rest:
        return -a
    for b in rest:
        a = a - b
    return a
@_coconut.functools.wraps(_coconut.itertools.tee)
def tee(iterable, n=2):
    if n >= 0 and _coconut.isinstance(iterable, (_coconut.tuple, _coconut.frozenset)):
        return (iterable,) * n
    if n > 0 and (_coconut.hasattr(iterable, "__copy__") or _coconut.isinstance(iterable, _coconut.abc.Sequence)):
        return (iterable,) + _coconut.tuple(_coconut.copy.copy(iterable) for _ in _coconut.range(n - 1))
    return _coconut.itertools.tee(iterable, n)
class reiterable(object):
    """Allows an iterator to be iterated over multiple times."""
    __slots__ = ("iter",)
    def __init__(self, iterable):
        self.iter = iterable
    def _get_new_iter(self):
        self.iter, new_iter = _coconut_tee(self.iter)
        return new_iter
    def __iter__(self):
        return _coconut.iter(self._get_new_iter())
    def __getitem__(self, index):
        return _coconut_igetitem(self._get_new_iter(), index)
    def __reversed__(self):
        return _coconut_reversed(self._get_new_iter())
    def __len__(self):
        return _coconut.len(self.iter)
    def __repr__(self):
        return "reiterable(%r)" % (self.iter,)
    def __reduce__(self):
        return (self.__class__, (self.iter,))
    def __copy__(self):
        return self.__class__(self._get_new_iter())
    def __fmap__(self, func):
        return _coconut_map(func, self)
class scan(object):
    """Reduce func over iterable, yielding intermediate results,
    optionally starting from initializer."""
    __slots__ = ("func", "iter", "initializer")
    def __init__(self, function, iterable, initializer=_coconut_sentinel):
        self.func = function
        self.iter = iterable
        self.initializer = initializer
    def __iter__(self):
        acc = self.initializer
        if acc is not _coconut_sentinel:
            yield acc
        for item in self.iter:
            if acc is _coconut_sentinel:
                acc = item
            else:
                acc = self.func(acc, item)
            yield acc
    def __len__(self):
        return _coconut.len(self.iter)
    def __repr__(self):
        return "scan(%r, %r)" % (self.func, self.iter)
    def __reduce__(self):
        return (self.__class__, (self.func, self.iter))
    def __copy__(self):
        return self.__class__(self.func, _coconut.copy.copy(self.iter))
    def __fmap__(self, func):
        return _coconut_map(func, self)
class reversed(object):
    __slots__ = ("iter",)
    if hasattr(_coconut.map, "__doc__"):
        __doc__ = _coconut.reversed.__doc__
    def __new__(cls, iterable):
        if _coconut.isinstance(iterable, _coconut.range):
            return iterable[::-1]
        if not _coconut.hasattr(iterable, "__reversed__") or _coconut.isinstance(iterable, (_coconut.list, _coconut.tuple)):
            return _coconut.object.__new__(cls)
        return _coconut.reversed(iterable)
    def __init__(self, iterable):
        self.iter = iterable
    def __iter__(self):
        return _coconut.iter(_coconut.reversed(self.iter))
    def __getitem__(self, index):
        if _coconut.isinstance(index, _coconut.slice):
            return _coconut_igetitem(self.iter, _coconut.slice(-(index.start + 1) if index.start is not None else None, -(index.stop + 1) if index.stop else None, -(index.step if index.step is not None else 1)))
        return _coconut_igetitem(self.iter, -(index + 1))
    def __reversed__(self):
        return self.iter
    def __len__(self):
        return _coconut.len(self.iter)
    def __repr__(self):
        return "reversed(%r)" % (self.iter,)
    def __hash__(self):
        return -_coconut.hash(self.iter)
    def __reduce__(self):
        return (self.__class__, (self.iter,))
    def __copy__(self):
        return self.__class__(_coconut.copy.copy(self.iter))
    def __eq__(self, other):
        return isinstance(other, self.__class__) and self.iter == other.iter
    def __contains__(self, elem):
        return elem in self.iter
    def count(self, elem):
        """Count the number of times elem appears in the reversed iterator."""
        return self.iter.count(elem)
    def index(self, elem):
        """Find the index of elem in the reversed iterator."""
        return _coconut.len(self.iter) - self.iter.index(elem) - 1
    def __fmap__(self, func):
        return self.__class__(_coconut_map(func, self.iter))
class map(_coconut.map):
    __slots__ = ("func", "iters")
    if hasattr(_coconut.map, "__doc__"):
        __doc__ = _coconut.map.__doc__
    def __new__(cls, function, *iterables):
        new_map = _coconut.map.__new__(cls, function, *iterables)
        new_map.func = function
        new_map.iters = iterables
        return new_map
    def __getitem__(self, index):
        if _coconut.isinstance(index, _coconut.slice):
            return self.__class__(self.func, *(_coconut_igetitem(i, index) for i in self.iters))
        return self.func(*(_coconut_igetitem(i, index) for i in self.iters))
    def __reversed__(self):
        return self.__class__(self.func, *(_coconut_reversed(i) for i in self.iters))
    def __len__(self):
        return _coconut.min(_coconut.len(i) for i in self.iters)
    def __repr__(self):
        return "map(%r, %s)" % (self.func, ", ".join((_coconut.repr(i) for i in self.iters)))
    def __reduce__(self):
        return (self.__class__, (self.func,) + self.iters)
    def __reduce_ex__(self, _):
        return self.__reduce__()
    def __copy__(self):
        return self.__class__(self.func, *_coconut.map(_coconut.copy.copy, self.iters))
    def __fmap__(self, func):
        return self.__class__(_coconut_forward_compose(self.func, func), *self.iters)
class parallel_map(map):
    """Multi-process implementation of map using concurrent.futures.
    Requires arguments to be pickleable."""
    __slots__ = ()
    def __iter__(self):
        from concurrent.futures import ProcessPoolExecutor
        with ProcessPoolExecutor() as executor:
            return _coconut.iter(_coconut.list(executor.map(self.func, *self.iters)))
    def __repr__(self):
        return "parallel_" + _coconut_map.__repr__(self)
class concurrent_map(map):
    """Multi-thread implementation of map using concurrent.futures."""
    __slots__ = ()
    def __iter__(self):
        from concurrent.futures import ThreadPoolExecutor
        from multiprocessing import cpu_count  # cpu_count() * 5 is the default Python 3.5 thread count
        with ThreadPoolExecutor(cpu_count() * 5) as executor:
            return _coconut.iter(_coconut.list(executor.map(self.func, *self.iters)))
    def __repr__(self):
        return "concurrent_" + _coconut_map.__repr__(self)
class filter(_coconut.filter):
    __slots__ = ("func", "iter")
    if hasattr(_coconut.filter, "__doc__"):
        __doc__ = _coconut.filter.__doc__
    def __new__(cls, function, iterable):
        new_filter = _coconut.filter.__new__(cls, function, iterable)
        new_filter.func = function
        new_filter.iter = iterable
        return new_filter
    def __reversed__(self):
        return self.__class__(self.func, _coconut_reversed(self.iter))
    def __repr__(self):
        return "filter(%r, %r)" % (self.func, self.iter)
    def __reduce__(self):
        return (self.__class__, (self.func, self.iter))
    def __reduce_ex__(self, _):
        return self.__reduce__()
    def __copy__(self):
        return self.__class__(self.func, _coconut.copy.copy(self.iter))
    def __fmap__(self, func):
        return _coconut_map(func, self)
class zip(_coconut.zip):
    __slots__ = ("iters",)
    if hasattr(_coconut.zip, "__doc__"):
        __doc__ = _coconut.zip.__doc__
    def __new__(cls, *iterables):
        new_zip = _coconut.zip.__new__(cls, *iterables)
        new_zip.iters = iterables
        return new_zip
    def __getitem__(self, index):
        if _coconut.isinstance(index, _coconut.slice):
            return self.__class__(*(_coconut_igetitem(i, index) for i in self.iters))
        return _coconut.tuple(_coconut_igetitem(i, index) for i in self.iters)
    def __reversed__(self):
        return self.__class__(*(_coconut_reversed(i) for i in self.iters))
    def __len__(self):
        return _coconut.min(_coconut.len(i) for i in self.iters)
    def __repr__(self):
        return "zip(%s)" % (", ".join((_coconut.repr(i) for i in self.iters)),)
    def __reduce__(self):
        return (self.__class__, self.iters)
    def __reduce_ex__(self, _):
        return self.__reduce__()
    def __copy__(self):
        return self.__class__(*_coconut.map(_coconut.copy.copy, self.iters))
    def __fmap__(self, func):
        return _coconut_map(func, self)
class enumerate(_coconut.enumerate):
    __slots__ = ("iter", "start")
    if hasattr(_coconut.enumerate, "__doc__"):
        __doc__ = _coconut.enumerate.__doc__
    def __new__(cls, iterable, start=0):
        new_enumerate = _coconut.enumerate.__new__(cls, iterable, start)
        new_enumerate.iter = iterable
        new_enumerate.start = start
        return new_enumerate
    def __getitem__(self, index):
        if _coconut.isinstance(index, _coconut.slice):
            return self.__class__(_coconut_igetitem(self.iter, index), self.start + (0 if index.start is None else index.start if index.start >= 0 else len(self.iter) + index.start))
        return (self.start + index, _coconut_igetitem(self.iter, index))
    def __len__(self):
        return _coconut.len(self.iter)
    def __repr__(self):
        return "enumerate(%r, %r)" % (self.iter, self.start)
    def __reduce__(self):
        return (self.__class__, (self.iter, self.start))
    def __reduce_ex__(self, _):
        return self.__reduce__()
    def __copy__(self):
        return self.__class__(_coconut.copy.copy(self.iter), self.start)
    def __fmap__(self, func):
        return _coconut_map(func, self)
class count(object):
    """count(start, step) returns an infinite iterator starting at start and increasing by step.
    If step is set to 0, count will infinitely repeat its first argument."""
    __slots__ = ("start", "step")
    def __init__(self, start=0, step=1):
        self.start = start
        self.step = step
    def __iter__(self):
        while True:
            yield self.start
            if self.step:
                self.start += self.step
    def __contains__(self, elem):
        if not self.step:
            return elem == self.start
        if elem < self.start:
            return False
        return (elem - self.start) % self.step == 0
    def __getitem__(self, index):
        if _coconut.isinstance(index, _coconut.slice) and (index.start is None or index.start >= 0) and (index.stop is None or index.stop >= 0):
            new_start, new_step = self.start, self.step
            if self.step and index.start is not None:
                new_start += self.step * index.start
            if self.step and index.step is not None:
                new_step *= index.step
            if index.stop is None:
                return self.__class__(new_start, new_step)
            if self.step and _coconut.isinstance(self.start, _coconut.int) and _coconut.isinstance(self.step, _coconut.int):
                return _coconut.range(new_start, self.start + self.step * index.stop, new_step)
            return _coconut_map(self.__getitem__, _coconut.range(index.start if index.start is not None else 0, index.stop, index.step if index.step is not None else 1))
        if index < 0:
            raise _coconut.IndexError("count indices must be positive")
        return self.start + self.step * index if self.step else self.start
    def count(self, elem):
        """Count the number of times elem appears in the count."""
        if not self.step:
            return _coconut.float("inf") if elem == self.start else 0
        return int(elem in self)
    def index(self, elem):
        """Find the index of elem in the count."""
        if elem not in self:
            raise _coconut.ValueError(_coconut.repr(elem) + " not in " + _coconut.repr(self))
        return (elem - self.start) // self.step if self.step else 0
    def __reversed__(self):
        if not self.step:
            return self
        raise _coconut.TypeError(repr(self) + " object is not reversible")
    def __repr__(self):
        return "count(%r, %r)" % (self.start, self.step)
    def __hash__(self):
        return _coconut.hash((self.start, self.step))
    def __reduce__(self):
        return (self.__class__, (self.start, self.step))
    def __copy__(self):
        return self.__class__(self.start, self.step)
    def __eq__(self, other):
        return isinstance(other, self.__class__) and self.start == other.start and self.step == other.step
    def __fmap__(self, func):
        return _coconut_map(func, self)
class groupsof(object):
    """groupsof(n, iterable) splits iterable into groups of size n.
    If the length of the iterable is not divisible by n, the last group may be of size < n."""
    __slots__ = ("group_size", "iter")
    def __init__(self, n, iterable):
        self.iter = iterable
        try:
            self.group_size = _coconut.int(n)
        except _coconut.ValueError:
            raise _coconut.TypeError("group size must be an int; not %r" % (n,))
        if self.group_size <= 0:
            raise _coconut.ValueError("group size must be > 0; not %r" % (self.group_size,))
    def __iter__(self):
        iterator = _coconut.iter(self.iter)
        loop = True
        while loop:
            group = []
            for _ in _coconut.range(self.group_size):
                try:
                    group.append(_coconut.next(iterator))
                except _coconut.StopIteration:
                    loop = False
                    break
            if group:
                yield _coconut.tuple(group)
    def __len__(self):
        return _coconut.len(self.iter)
    def __repr__(self):
        return "groupsof(%r)" % (self.iter,)
    def __reduce__(self):
        return (self.__class__, (self.group_size, self.iter))
    def __copy__(self):
        return self.__class__(self.group_size, _coconut.copy.copy(self.iter))
    def __fmap__(self, func):
        return _coconut_map(func, self)
class recursive_iterator(object):
    """Decorator that optimizes a function for iterator recursion."""
    __slots__ = ("func", "tee_store", "backup_tee_store")
    def __init__(self, func):
        self.func = func
        self.tee_store = {}
        self.backup_tee_store = []
    def __call__(self, *args, **kwargs):
        key = (args, _coconut.frozenset(kwargs))
        use_backup = False
        try:
            hash(key)
        except _coconut.Exception:
            try:
                key = _coconut.pickle.dumps(key, -1)
            except _coconut.Exception:
                use_backup = True
        if use_backup:
            for i, (k, v) in _coconut.enumerate(self.backup_tee_store):
                if k == key:
                    to_tee, store_pos = v, i
                    break
            else:  # no break
                to_tee = self.func(*args, **kwargs)
                store_pos = None
            to_store, to_return = _coconut_tee(to_tee)
            if store_pos is None:
                self.backup_tee_store.append([key, to_store])
            else:
                self.backup_tee_store[store_pos][1] = to_store
        else:
            self.tee_store[key], to_return = _coconut_tee(self.tee_store.get(key) or self.func(*args, **kwargs))
        return to_return
    def __repr__(self):
        return "@recursive_iterator(" + _coconut.repr(self.func) + ")"
    def __reduce__(self):
        return (self.__class__, (self.func,))
    def __get__(self, obj, objtype=None):
        return _coconut.functools.partial(self, obj)
class _coconut_FunctionMatchErrorContext(object):
    __slots__ = ('exc_class', 'taken')
    threadlocal_var = _coconut.threading.local()
    def __init__(self, exc_class):
        self.exc_class = exc_class
        self.taken = False
    def __enter__(self):
        try:
            self.threadlocal_var.contexts.append(self)
        except _coconut.AttributeError:
            self.threadlocal_var.contexts = [self]
    def __exit__(self, type, value, traceback):
        self.threadlocal_var.contexts.pop()
    @classmethod
    def get(cls):
        try:
            ctx = cls.threadlocal_var.contexts[-1]
        except (_coconut.AttributeError, _coconut.IndexError):
            return _coconut_MatchError
        if not ctx.taken:
            ctx.taken = True
            return ctx.exc_class
        return _coconut_MatchError
_coconut_get_function_match_error = _coconut_FunctionMatchErrorContext.get
class _coconut_base_pattern_func(object):
    __slots__ = ("FunctionMatchError", "__doc__", "patterns")
    def __init__(self, *funcs):
        self.FunctionMatchError = _coconut.type(_coconut_str("MatchError"), (_coconut_MatchError,), {})
        self.__doc__ = None
        self.patterns = []
        for func in funcs:
            self.add(func)
    def add(self, func):
        self.__doc__ = _coconut.getattr(func, "__doc__", None) or self.__doc__
        if _coconut.isinstance(func, _coconut_base_pattern_func):
            self.patterns += func.patterns
        else:
            self.patterns.append(func)
    def __call__(self, *args, **kwargs):
        for func in self.patterns[:-1]:
            try:
                with _coconut_FunctionMatchErrorContext(self.FunctionMatchError):
                    return func(*args, **kwargs)
            except self.FunctionMatchError:
                pass
        return self.patterns[-1](*args, **kwargs)
    def _coconut_tco_func(self, *args, **kwargs):
        for func in self.patterns[:-1]:
            try:
                with _coconut_FunctionMatchErrorContext(self.FunctionMatchError):
                    return func(*args, **kwargs)
            except self.FunctionMatchError:
                pass
        return _coconut_tail_call(self.patterns[-1], *args, **kwargs)
    def __repr__(self):
        return "addpattern(" + _coconut.repr(self.patterns[0]) + ")(*" + _coconut.repr(self.patterns[1:]) + ")"
    def __reduce__(self):
        return (self.__class__, _coconut.tuple(self.patterns))
    def __get__(self, obj, objtype=None):
        return _coconut.functools.partial(self, obj)
def addpattern(base_func):
    """Decorator to add a new case to a pattern-matching function,
    where the new case is checked last."""
    return _coconut.functools.partial(_coconut_base_pattern_func, base_func)
_coconut_addpattern = addpattern
def prepattern(base_func):
    """DEPRECATED: Use addpattern instead."""
    def pattern_prepender(func):
        return addpattern(func)(base_func)
    return pattern_prepender
class _coconut_partial(object):
    __slots__ = ("func", "_argdict", "_arglen", "_stargs", "keywords")
    if hasattr(_coconut.functools.partial, "__doc__"):
        __doc__ = _coconut.functools.partial.__doc__
    def __init__(self, func, argdict, arglen, *args, **kwargs):
        self.func = func
        self._argdict = argdict
        self._arglen = arglen
        self._stargs = args
        self.keywords = kwargs
    def __reduce__(self):
        return (self.__class__, (self.func, self._argdict, self._arglen) + self._stargs, self.keywords)
    def __setstate__(self, keywords):
        self.keywords = keywords
    @property
    def args(self):
        return _coconut.tuple(self._argdict.get(i) for i in _coconut.range(self._arglen)) + self._stargs
    def __call__(self, *args, **kwargs):
        callargs = []
        argind = 0
        for i in _coconut.range(self._arglen):
            if i in self._argdict:
                callargs.append(self._argdict[i])
            elif argind >= _coconut.len(args):
                raise _coconut.TypeError("expected at least " + _coconut.str(self._arglen - _coconut.len(self._argdict)) + " argument(s) to " + _coconut.repr(self))
            else:
                callargs.append(args[argind])
                argind += 1
        callargs += self._stargs
        callargs += args[argind:]
        kwargs.update(self.keywords)
        return self.func(*callargs, **kwargs)
    def __repr__(self):
        args = []
        for i in _coconut.range(self._arglen):
            if i in self._argdict:
                args.append(_coconut.repr(self._argdict[i]))
            else:
                args.append("?")
        for arg in self._stargs:
            args.append(_coconut.repr(arg))
        return _coconut.repr(self.func) + "$(" + ", ".join(args) + ")"
def consume(iterable, keep_last=0):
    """consume(iterable, keep_last) fully exhausts iterable and return the last keep_last elements."""
    return _coconut.collections.deque(iterable, maxlen=keep_last)
class starmap(_coconut.itertools.starmap):
    __slots__ = ("func", "iter")
    if hasattr(_coconut.itertools.starmap, "__doc__"):
        __doc__ = _coconut.itertools.starmap.__doc__
    def __new__(cls, function, iterable):
        new_map = _coconut.itertools.starmap.__new__(cls, function, iterable)
        new_map.func = function
        new_map.iter = iterable
        return new_map
    def __getitem__(self, index):
        if _coconut.isinstance(index, _coconut.slice):
            return self.__class__(self.func, _coconut_igetitem(self.iter, index))
        return self.func(*_coconut_igetitem(self.iter, index))
    def __reversed__(self):
        return self.__class__(self.func, *_coconut_reversed(self.iter))
    def __len__(self):
        return _coconut.len(self.iter)
    def __repr__(self):
        return "starmap(%r, %r)" % (self.func, self.iter)
    def __reduce__(self):
        return (self.__class__, (self.func, self.iter))
    def __reduce_ex__(self, _):
        return self.__reduce__()
    def __copy__(self):
        return self.__class__(self.func, _coconut.copy.copy(self.iter))
    def __fmap__(self, func):
        return self.__class__(_coconut_forward_compose(self.func, func), self.iter)
def makedata(data_type, *args):
    """Construct an object of the given data_type containing the given arguments."""
    if _coconut.hasattr(data_type, "_make") and _coconut.issubclass(data_type, _coconut.tuple):
        return data_type._make(args)
    if _coconut.issubclass(data_type, (_coconut.map, _coconut.range, _coconut.abc.Iterator)):
        return args
    if _coconut.issubclass(data_type, _coconut.str):
        return "".join(args)
    return data_type(args)
def datamaker(data_type):
    """DEPRECATED: Use makedata instead."""
    return _coconut.functools.partial(makedata, data_type)
def fmap(func, obj):
    """fmap(func, obj) creates a copy of obj with func applied to its contents.
    Override by defining obj.__fmap__(func)."""
    if _coconut.hasattr(obj, "__fmap__"):
        return obj.__fmap__(func)
    if obj.__class__.__module__ == "numpy":
        from numpy import vectorize
        return vectorize(func)(obj)
    return _coconut_makedata(obj.__class__, *(_coconut_starmap(func, obj.items()) if _coconut.isinstance(obj, _coconut.abc.Mapping) else _coconut_map(func, obj)))
def memoize(maxsize=None, *args, **kwargs):
    """Decorator that memoizes a function,
    preventing it from being recomputed if it is called multiple times with the same arguments."""
    return _coconut.functools.lru_cache(maxsize, *args, **kwargs)
_coconut_MatchError, _coconut_count, _coconut_enumerate, _coconut_makedata, _coconut_map, _coconut_reversed, _coconut_starmap, _coconut_tee, _coconut_zip, TYPE_CHECKING, reduce, takewhile, dropwhile = MatchError, count, enumerate, makedata, map, reversed, starmap, tee, zip, False, _coconut.functools.reduce, _coconut.itertools.takewhile, _coconut.itertools.dropwhile

# Compiled Coconut: -----------------------------------------------------------

import coconut.convenience
import pandas as pd
import numpy as np
from pandas import DataFrame
from pandas import Series
from pandas.core.groupby.generic import DataFrameGroupBy
from typing import List
from typing import Tuple
from typing import Dict
from typing import Any

spam_analysis_data = {'SpamId': [376, 489, 541, 693, 782, 976], 'SuspiciousWords': [True, True, True, False, False, False], 'UnknownSender': [False, True, True, True, False, False], 'Images': [True, False, False, True, False, False], 'SpamClass': ["spam", "spam", "spam", "ham", "ham", "ham"]}


ecological_vegetation_data = {'Id': [1, 2, 3, 4, 5, 6, 7], 'Stream': [False, True, True, False, False, True, True], 'Slope': ['steep', 'moderate', 'steep', 'steep', 'flat', 'steep', 'steep'], 'Elevation': ['high', 'low', 'medium', 'medium', 'high', 'highest', 'high'], 'Vegetation': ['chaparal', 'riparian', 'riparian', 'chaparal', 'conifer', 'conifer', 'chaparal']}


def entropy(total_records,  # type: int
     value_frequencies,  # type: np.array
     log_base=2  # type: int
    ):
# type: (...) -> float
    item_probs = value_frequencies / total_records
    return -(item_probs * np.log(item_probs) / np.log(log_base)).sum()


@_coconut_tco
def frame_entropy(df,  # type: DataFrame
     target_feature  # type: str
    ):
# type: (...) -> float
    grouped_df = df.groupby(target_feature)
    counts = map(lambda k: len(grouped_df.get_group(k).index), grouped_df.indices.keys())
    return _coconut_tail_call(entropy, len(df.index), (np.array)((list)(counts)))


@_coconut_tco
def remaining_entropy(original_df,  # type: DataFrame
     target_feature,  # type: str
     grouped_df  # type: DataFrameGroupBy
    ):
# type: (...) -> float
    def weighted_group_entropy(df  # type: DataFrame
    ):
# type: (...) -> float
        return (len(df.index) / len(original_df.index) * frame_entropy(df, target_feature))
    grouped_frames = map(grouped_df.get_group, grouped_df.indices.keys())
    return _coconut_tail_call(((np.array)((list)(map(weighted_group_entropy, grouped_frames)))).sum)


def information_gain(target_feature,  # type: str
     original_entropy,  # type: float
     original_df,  # type: DataFrame
     grouped_df  # type: DataFrameGroupBy
    ):
# type: (...) -> float
    return original_entropy - remaining_entropy(original_df, target_feature, grouped_df)


# Change this function to return a data object.
@_coconut_tco
def find_most_informative_feature(target_feature,  # type: str
     df  # type: DataFrame
    ):
# type: (...) -> (str, float, DataFrameGroupBy)
    original_entropy = frame_entropy(df, target_feature)
    def calc_IG(descriptive_feature  # type: str
    ):
# type: (...) -> (str, float, DataFrameGroupBy)
        grouped_df = df.groupby(descriptive_feature)
        return (descriptive_feature, information_gain(target_feature, original_entropy, df, grouped_df), grouped_df)

    def keep_greatest_information_gain(acc_df,  # type: (float, str, DataFrameGroupBy)
     next_descriptive_feature  # type: str
    ):
# type: (...) -> (str, float, DataFrameGroupBy)
        next_df = calc_IG(next_descriptive_feature)
        return acc_df if acc_df[1] >= next_df[1] else next_df

    descriptive_features = (list)(df.drop(target_feature, axis=1).columns)
    descriptive_features[0] = calc_IG(descriptive_features[0])
    return _coconut_tail_call(reduce, keep_greatest_information_gain, descriptive_features)


def id3(target_feature,  # type: str
     df  # type: DataFrame
    ):
# type: (...) -> Tuple[str, Dict[Any, Any]]
    """
    High-level algorithm summary:

    1. If all of the target_feature values in the training set DataFrame are the
       same value, return that value as the new leaf.
    2. If there is only one descriptive_feature left to split on, split by it and
       make a new node which is a tuple where the first tuple value is the name
       of the descriptive_feature column, and the second is a dictionary where
       the keys are each unique value of the descriptive_feature column, and the
       values are the mode of the target_feature of that grouping of the
       descriptive_feature value.  Also add an `otherwise` key whose value is
       the mode of the unsplit frame's target_feature column in case real-world
       data contains unique values of the descriptive feature that got excluded
       via former iterations of splitting the training set to build this model.
    3. Otherwise, identify the descriptive_feature which yields the greatest
       Information Gain and use it to split the training set DataFrame.  Make
       a new node which is a tuple where the first tuple value is the name of
       the descriptive_feature column, and the second is the a dictionary where
       the keys are each unique value of the descriptive_feature column, and
       the values are the result of running this id3 function recursively over
       the DataFrame for the group at the key (which is the descriptive_feature
       value).  As in step 2, also add an `otherwise` key whose value is
       the mode of the unsplit frame's target_feature column in case real-world
       data contains unique values of the descriptive feature that got excluded
       via former iterations of splitting the training set to build this model.

    :param target_feature: The name of the column this model should try to
        predict.
    :param df: The training set to use to build this decision tree model.

    :return: A tuple which is a decision tree structure.  To explain the
        mypy type signature, the Dict's first Any represents the type of
        the descriptive_feature at that node, and the second Any could be either
        another tree node, which would have signature Tuple[str, Dict[Any, Any]],
        or it could be a leaf, which would just be a str value which is the value
        predicted at the end of that traversal of the tree.

        Here are some sample trees:
        # simple Ham or Spam prediction example
        ("SuspiciousWords", {True: 'spam', False: 'ham'})

        # sample Ecological Vegetation decision tree
        ("Elevation", {
            "low": "riparian",
            "highest": "conifer",
            "medium": ("Stream, {
                True: "riparian",
                False: "chaparal"
            }),
            "high": ("Slope", {
                "flat": "conifer",
                "steep": "chaparal",
                # below, "moderate" was eliminated through splitting, so the
                # mode of the target_level of the frame unsplit by "Slope" is
                # assumed via "otherwise" for any value other than "flat" or
                # "steep", which covers "moderate" since it got excluded by
                # the split.
                "otherwise": "chaparal" 
            })
        })
    """
    unique_target_values = df[target_feature].unique()
    if len(unique_target_values) == 1:
        new_node = unique_target_values[0]  # a leaf
    else:  # Can skip the rest if the previous if conditional was True
        descriptive_features = (list)(df.columns)
        descriptive_features.remove(target_feature)

        if len(descriptive_features) == 1:  # TODO: pull this check into find_most_informative_feature.
# Implies we're on the last descriptive feature.  Split the frame and make
# the next node at each split the mode of the target_feature column after the split.
            last_descriptive_feature = descriptive_features[0]
            grouped_df = df.groupby(last_descriptive_feature)  # type: DataFrameGroupBy
            new_leaves = dict(((key), (grouped_df.get_group(key)[target_feature].mode())) for key in grouped_df.indices.keys())
# The below update is done in case unique values of the descriptive feature that
# got excluded through splitting the training set turn up in the real data.  In
# this case, the current unsplit mode of the target_feature is assumed to be the most
# accurate prediction.
            new_leaves.update({'otherwise': df[target_feature].mode()})
            new_node = (last_descriptive_feature, new_leaves)
        else:
# Each node will have the column name, the mode at this level, and a list
# of tuples where the first tuple value is a column value and the second
# is the next Frame to run id3 against.
            best_feature = find_most_informative_feature(target_feature, df)
            print(best_feature)  # for debugging purposes...
            grouped_df = best_feature[-1]  # type: DataFrameGroupBy
# The default 'otherwise' is done in case unique values of the descriptive feature that
# got excluded through splitting the training set turn up in the real data.  In
# this case, the current unsplit mode of the target_feature is assumed to be the most
# accurate prediction.
            new_leaves = {'otherwise': df[target_feature].mode()}
            new_leaves.update(dict(((key), (id3(target_feature, grouped_df.get_group(key)))) for key in grouped_df.indices.keys()))
            new_node = (best_feature[0], new_leaves)
    return new_node


# def run_dtree_model(dtree: tuple, row: Series) -> Series =
# df.apply(lambda r: pd.concat([r, pd.Series(["happy"])]), axis=1) 


spam_analysis_df = pd.DataFrame(spam_analysis_data).drop('SpamId', axis=1)
(print)(id3('SpamClass', spam_analysis_df))

ecological_vegetation_df = pd.DataFrame(ecological_vegetation_data).drop('Id', axis=1)
(print)(id3('Vegetation', ecological_vegetation_df))

print()
print('----------------------------------------------------------------------')
print()
# The following dataset comes from https://archive.ics.uci.edu/ml/datasets/Acute+Inflammations
acute_inflammations_df = pd.read_csv('../../../datasets/acute_diagnoses/diagnosis.data', sep='\t', lineterminator='\n', header=None, encoding='utf-8')
acute_inflammations_df[7] = acute_inflammations_df[7].str.strip('\r')
acute_inflammations_df = acute_inflammations_df.rename(columns={0: 'Temperature', 1: 'Nausea', 2: 'LumbarPain', 3: 'UrinePushing', 4: 'MicturationPains', 5: 'UrethraBurning', 6: 'BladderInflammation', 7: 'RenalPelvisNephritis'})
acute_inflammations_id3_df = acute_inflammations_df.drop('Temperature', axis=1)
acute_inflammations_id3_predict_bladder_inflammation_df = (acute_inflammations_id3_df.drop('RenalPelvisNephritis', axis=1))

((print)(id3('BladderInflammation', acute_inflammations_id3_predict_bladder_inflammation_df)))

acute_inflammations_id3_predict_nephritis_df = acute_inflammations_id3_df.drop('BladderInflammation', axis=1)
((print)(id3('RenalPelvisNephritis', acute_inflammations_id3_predict_nephritis_df)))
