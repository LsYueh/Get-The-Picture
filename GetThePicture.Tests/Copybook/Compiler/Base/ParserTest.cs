using System.Text;
using GetThePicture.Copybook.Compiler.Base;
using GetThePicture.Copybook.Compiler.Layout;
using GetThePicture.Copybook.Compiler.Layout.Item;

namespace GetThePicture.Tests.Copybook.Compiler.Base;

[TestClass]

public class ParserTest
{
    private static readonly Lexer lexer = new();

    [TestMethod]
    public void Semantic_Analysis_Test_01()
    {        
        string line = "05 CUSTOMER-NAME PIC X(10).";

        var tokens = lexer.Tokenize(line, 1).ToList();

        Parser parser = new(tokens);

        var model = parser.Analyze();
        
        Assert.IsNotNull(model);
    }

    [TestMethod]
    public void Semantic_Analysis_Test_02()
    {        
        string line = "05 CUSTOMER-NAME PIC X(10) VALUE 'ABC'.";

        var tokens = lexer.Tokenize(line, 1).ToList();

        Parser parser = new(tokens);

        var model = parser.Analyze();
        
        Assert.IsNotNull(model);
    }

    [TestMethod]
    public void Semantic_Analysis_Test_03()
    {        
        string line = "05 MONTH-NAME PIC X(3) OCCURS 12 TIMES VALUE \"---\".";

        var tokens = lexer.Tokenize(line, 1).ToList();

        Parser parser = new(tokens);

        var model = parser.Analyze();
        
        Assert.IsNotNull(model);
    }
    
    [TestMethod]
    public void Semantic_Analysis_Test_04()
    {        
        string line = @"
 01  CLIRTVO-REC.
           03 MESSAGE-HEADER.
               05 MSGIDA                       PIC  X(030).
        ";

        var tokens = lexer.Tokenize(line, 1).ToList();

        Parser parser = new(tokens);

        CbLayout layout = parser.Analyze();
        Assert.IsNotNull(layout);
        Assert.AreEqual(0, layout.Level);
        Assert.IsNotNull(layout.Children);
        
        GroupItem? groupItem_01 = (GroupItem?) layout.Children[0];
        Assert.IsNotNull(groupItem_01);
        Assert.AreEqual(1, groupItem_01.Level);
        Assert.IsNotNull(groupItem_01.Children);

        GroupItem? subordinate_03 =  (GroupItem?) groupItem_01.Children[0];
        Assert.IsNotNull(subordinate_03);
        Assert.AreEqual(3, subordinate_03.Level);
        Assert.IsNotNull(subordinate_03.Children);

        ElementaryDataItem? subordinate_05 =  (ElementaryDataItem?) subordinate_03.Children[0];
        Assert.IsNotNull(subordinate_05);
        Assert.AreEqual(5, subordinate_05.Level);
        Assert.IsNotNull(subordinate_05.Pic);
    }

    [TestMethod]
    public void Semantic_Analysis_Test_05()
    {        
        string line = @"
 01  CLIRTVO-REC.
           03 MESSAGE-HEADER.
               05 MSGIDA                       PIC  X(030).
               05 MSGLNG                       PIC  9(005).
               05 MSGCNT                       PIC  S9(004)V9(4).
               05 FILLER                       PIC  X(010).
               05 MSGID                        PIC  X(010).
";

        var tokens = lexer.Tokenize(line, 1).ToList();

        Parser parser = new(tokens);

        CbLayout layout = parser.Analyze();
        Assert.IsNotNull(layout);
        Assert.AreEqual(0, layout.Level);
        Assert.IsNotNull(layout.Children);

        GroupItem? groupItem_01 = (GroupItem?) layout.Children[0];
        Assert.IsNotNull(groupItem_01);
        Assert.AreEqual(1, groupItem_01.Level);
        Assert.IsNotNull(groupItem_01.Children);

        GroupItem? subordinate_03 =  (GroupItem?) groupItem_01.Children[0];
        Assert.IsNotNull(subordinate_03);
        Assert.AreEqual(3, subordinate_03.Level);
        Assert.AreEqual(5, subordinate_03.Children.Count);

        ElementaryDataItem? subordinate_05 =  (ElementaryDataItem?) subordinate_03.Children[3];
        Assert.IsNotNull(subordinate_05);
        Assert.AreEqual(5, subordinate_05.Level);
        Assert.IsNotNull(subordinate_05.Pic);
        Assert.IsTrue(subordinate_05.IsFiller);
    }

    [TestMethod]
    public void Semantic_Analysis_Test_06()
    {        
        string line = @"
  01  MAILING-RECORD.
           05  COMPANY-NAME            PIC X(30).
           05  CONTACTS.
               10  PRESIDENT.
                   15  LAST-NAME       PIC X(15).
                   15  FIRST-NAME      PIC X(8).
               10  VP-MARKETING.
                   15  LAST-NAME       PIC X(15).
                   15  FIRST-NAME      PIC X(8).
               10  ALTERNATE-CONTACT.
                   15  TITLE           PIC X(10).
                   15  LAST-NAME       PIC X(15).
                   15  FIRST-NAME      PIC X(8).
           05  ADDRESS                 PIC X(15).
           05  CITY                    PIC X(15).
           05  STATE                   PIC X(2).
           05  ZIP                     PIC 9(5).
";

        var tokens = lexer.Tokenize(line, 1).ToList();

        Parser parser = new(tokens);

        CbLayout layout = parser.Analyze();
        Assert.IsNotNull(layout);
        Assert.AreEqual(0, layout.Level);
        Assert.IsNotNull(layout.Children);

        GroupItem? groupItem_01 = (GroupItem?) layout.Children[0];
        Assert.IsNotNull(groupItem_01);
        Assert.AreEqual(1, groupItem_01.Level);
        Assert.IsNotNull(groupItem_01.Children);
        
        GroupItem? subordinate_05 =  (GroupItem?) groupItem_01.Children[1];
        Assert.IsNotNull(subordinate_05);
        Assert.AreEqual(5, subordinate_05.Level);
        Assert.AreEqual(3, subordinate_05.Children.Count);

        GroupItem? subordinate_10 =  (GroupItem?) subordinate_05.Children[2];
        Assert.IsNotNull(subordinate_10);
        Assert.AreEqual(10, subordinate_10.Level);
        Assert.AreEqual("ALTERNATE-CONTACT", subordinate_10.Name);
        Assert.AreEqual(3, subordinate_10.Children.Count);

        ElementaryDataItem? subordinate_15 =  (ElementaryDataItem?) subordinate_10.Children[2];
        Assert.IsNotNull(subordinate_15);
        Assert.AreEqual(15, subordinate_15.Level);
        Assert.IsNotNull(subordinate_15.Pic);
        Assert.IsFalse(subordinate_15.IsFiller);
    }

    [TestMethod]
    public void Semantic_Analysis_Test_07()
    {        
        string line = "05 FILLER                PIC 9(10) VALUE ZEROS.";

        var tokens = lexer.Tokenize(line, 1).ToList();

        Parser parser = new(tokens);

        var layout = parser.Analyze();
        layout.Seal();

        Assert.IsNotNull(layout);

        var sb = new StringBuilder();
        using var writer = new StringWriter(sb);

        layout.Dump(writer);

        string result = sb.ToString();

        StringAssert.Contains(result, "5 FILLER >> PIC: [9(10)] Class='Numeric' (Semantic='None'), Signed=False, Int=10, Dec=0, Len=10, Usage='Display' VALUE: \"0\"");
    }
}