using NUnit.Framework;

namespace IronMeta.Tests
{
	[TestFixture]
	public class Tratt5
	{
		[Test]
		public void testTratt5()
		{
			var c = new Calc();
			var match = c.GetMatch("1-2-3", c.Expression);
			Assert.True(match.Success);
			Assert.That(match.Result, Is.EqualTo("(1-(2-3))"));
		}
	}
}
