with Ahven.Text_Runner;
with Ahven.Framework;

--  with ESL.Client.Test;
with ESL.Command.Test;
with ESL.Job.List.Test;

procedure Tester is
   S : Ahven.Framework.Test_Suite := Ahven.Framework.Create_Suite ("All");

begin
   Ahven.Framework.Add_Test (Suite => S, T => new ESL.Job.List.Test.Instance);
--   Ahven.Framework.Add_Test (Suite => S, T => new ESL.Client.Test.Instance);
   Ahven.Framework.Add_Test (Suite => S, T => new ESL.Command.Test.Instance);
   Ahven.Text_Runner.Run (S);
end Tester;
