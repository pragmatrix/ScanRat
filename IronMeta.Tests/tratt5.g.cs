//
// IronMeta Calc Parser; Generated 2013-08-13 18:15:47Z UTC
//

using System;
using System.Collections.Generic;
using System.Linq;
using IronMeta.Matcher;

#pragma warning disable 1591

namespace IronMeta.Tests
{

    using _Calc_Inputs = IEnumerable<char>;
    using _Calc_Results = IEnumerable<string>;
    using _Calc_Item = IronMeta.Matcher.MatchItem<char, string>;
    using _Calc_Args = IEnumerable<IronMeta.Matcher.MatchItem<char, string>>;
    using _Calc_Memo = Memo<char, string>;
    using _Calc_Rule = System.Action<Memo<char, string>, int, IEnumerable<IronMeta.Matcher.MatchItem<char, string>>>;
    using _Calc_Base = IronMeta.Matcher.Matcher<char, string>;

    public partial class Calc : IronMeta.Matcher.CharMatcher<string>
    {
        public Calc()
            : base()
        { }

        public Calc(bool handle_left_recursion)
            : base(handle_left_recursion)
        { }

        public void Expression(_Calc_Memo _memo, int _index, _Calc_Args _args)
        {

            _Calc_Item a = null;
            _Calc_Item b = null;

            // OR 0
            int _start_i0 = _index;

            // AND 2
            int _start_i2 = _index;

            // AND 3
            int _start_i3 = _index;

            // CALLORVAR Expression
            _Calc_Item _r5;

            _r5 = _MemoCall(_memo, "Expression", _index, Expression, null);

            if (_r5 != null) _index = _r5.NextIndex;

            // BIND a
            a = _memo.Results.Peek();

            // AND shortcut
            if (_memo.Results.Peek() == null) { _memo.Results.Push(null); goto label3; }

            // LITERAL "-"
            _ParseLiteralString(_memo, ref _index, "-");

        label3: // AND
            var _r3_2 = _memo.Results.Pop();
            var _r3_1 = _memo.Results.Pop();

            if (_r3_1 != null && _r3_2 != null)
            {
                _memo.Results.Push( new _Calc_Item(_start_i3, _index, _memo.InputEnumerable, _r3_1.Results.Concat(_r3_2.Results).Where(_NON_NULL), true) );
            }
            else
            {
                _memo.Results.Push(null);
                _index = _start_i3;
            }

            // AND shortcut
            if (_memo.Results.Peek() == null) { _memo.Results.Push(null); goto label2; }

            // CALLORVAR Expression
            _Calc_Item _r8;

            _r8 = _MemoCall(_memo, "Expression", _index, Expression, null);

            if (_r8 != null) _index = _r8.NextIndex;

            // BIND b
            b = _memo.Results.Peek();

        label2: // AND
            var _r2_2 = _memo.Results.Pop();
            var _r2_1 = _memo.Results.Pop();

            if (_r2_1 != null && _r2_2 != null)
            {
                _memo.Results.Push( new _Calc_Item(_start_i2, _index, _memo.InputEnumerable, _r2_1.Results.Concat(_r2_2.Results).Where(_NON_NULL), true) );
            }
            else
            {
                _memo.Results.Push(null);
                _index = _start_i2;
            }

            // ACT
            var _r1 = _memo.Results.Peek();
            if (_r1 != null)
            {
                _memo.Results.Pop();
                _memo.Results.Push( new _Calc_Item(_r1.StartIndex, _r1.NextIndex, _memo.InputEnumerable, _Thunk(_IM_Result => { return "(" + a + "-" + b + ")"; }, _r1), true) );
            }

            // OR shortcut
            if (_memo.Results.Peek() == null) { _memo.Results.Pop(); _index = _start_i0; } else goto label0;

            // CALLORVAR DecimalDigit
            _Calc_Item _r9;

            _r9 = _MemoCall(_memo, "DecimalDigit", _index, DecimalDigit, null);

            if (_r9 != null) _index = _r9.NextIndex;

        label0: // OR
            int _dummy_i0 = _index; // no-op for label

        }


        public void DecimalDigit(_Calc_Memo _memo, int _index, _Calc_Args _args)
        {

            _Calc_Item c = null;

            // INPUT CLASS
            _ParseInputClass(_memo, ref _index, '\u0030', '\u0031', '\u0032', '\u0033', '\u0034', '\u0035', '\u0036', '\u0037', '\u0038', '\u0039');

            // BIND c
            c = _memo.Results.Peek();

            // ACT
            var _r0 = _memo.Results.Peek();
            if (_r0 != null)
            {
                _memo.Results.Pop();
                _memo.Results.Push( new _Calc_Item(_r0.StartIndex, _r0.NextIndex, _memo.InputEnumerable, _Thunk(_IM_Result => { return ((char)c - '0').ToString(); }, _r0), true) );
            }

        }

    } // class Calc

} // namespace IronMeta.Tests

