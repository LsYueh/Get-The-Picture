using GetThePicture.Copybook.Compiler;
using GetThePicture.Copybook.Compiler.Ir;

namespace GetThePicture.Tests.Copybook.Compiler;

[TestClass]

public class ParserTest
{
    [TestMethod]
    public void Semantic_Analysis_Test_01()
    {        
        string line = "05 CUSTOMER-NAME PIC X(10).";

        var tokens = Lexer.Tokenize(line, 1).ToList();

        Parser parser = new(tokens);

        var model = parser.Analyze();
        
        Assert.IsNotNull(model);
    }

    [TestMethod]
    public void Semantic_Analysis_Test_02()
    {        
        string line = "05 CUSTOMER-NAME PIC X(10) VALUE 'ABC'.";

        var tokens = Lexer.Tokenize(line, 1).ToList();

        Parser parser = new(tokens);

        var model = parser.Analyze();
        
        Assert.IsNotNull(model);
    }

    [TestMethod]
    public void Semantic_Analysis_Test_03()
    {        
        string line = "05 MONTH-NAME PIC X(3) OCCURS 12 TIMES VALUE \"---\".";

        var tokens = Lexer.Tokenize(line, 1).ToList();

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

        var tokens = Lexer.Tokenize(line, 1).ToList();

        Parser parser = new(tokens);

        GroupItem? root = (GroupItem?) parser.Analyze();
        
        Assert.IsNotNull(root);
        Assert.AreEqual(1, root.Level);
        Assert.IsNotNull(root.Subordinates);

        GroupItem? subordinate_03 =  (GroupItem?) root.Subordinates[0];
        Assert.IsNotNull(subordinate_03);
        Assert.AreEqual(3, subordinate_03.Level);
        Assert.IsNotNull(subordinate_03.Subordinates);

        ElementaryDataItem? subordinate_05 =  (ElementaryDataItem?) subordinate_03.Subordinates[0];
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

        var tokens = Lexer.Tokenize(line, 1).ToList();

        Parser parser = new(tokens);

        GroupItem? root = (GroupItem?) parser.Analyze();
        
        Assert.IsNotNull(root);
        Assert.AreEqual(1, root.Level);
        Assert.IsNotNull(root.Subordinates);

        GroupItem? subordinate_03 =  (GroupItem?) root.Subordinates[0];
        Assert.IsNotNull(subordinate_03);
        Assert.AreEqual(3, subordinate_03.Level);
        Assert.AreEqual(5, subordinate_03.Subordinates.Count);

        ElementaryDataItem? subordinate_05 =  (ElementaryDataItem?) subordinate_03.Subordinates[3];
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

        var tokens = Lexer.Tokenize(line, 1).ToList();

        Parser parser = new(tokens);

        GroupItem? root = (GroupItem?) parser.Analyze();
        
        Assert.IsNotNull(root);
        Assert.AreEqual(1, root.Level);
        Assert.IsNotNull(root.Subordinates);
        Assert.AreEqual(6, root.Subordinates.Count);
        
        GroupItem? subordinate_05 =  (GroupItem?) root.Subordinates[1];
        Assert.IsNotNull(subordinate_05);
        Assert.AreEqual(5, subordinate_05.Level);
        Assert.AreEqual(3, subordinate_05.Subordinates.Count);

        GroupItem? subordinate_10 =  (GroupItem?) subordinate_05.Subordinates[2];
        Assert.IsNotNull(subordinate_10);
        Assert.AreEqual(10, subordinate_10.Level);
        Assert.AreEqual("ALTERNATE-CONTACT", subordinate_10.Name);
        Assert.AreEqual(3, subordinate_10.Subordinates.Count);

        ElementaryDataItem? subordinate_15 =  (ElementaryDataItem?) subordinate_10.Subordinates[2];
        Assert.IsNotNull(subordinate_15);
        Assert.AreEqual(15, subordinate_15.Level);
        Assert.IsNotNull(subordinate_15.Pic);
        Assert.IsFalse(subordinate_15.IsFiller);
    }
}