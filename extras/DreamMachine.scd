(
{
	var machine;
	Server.default.bootSync;
	DM_Synth.addDef;
	Server.default.sync;
	machine = DM_Synth();
	{machine.makeInterface}.defer;
	// m = machine;
}.fork
)

(
{
	var machines;
	Server.default.bootSync;
	DM_Synth.addDef;
	Server.default.sync;
	machines = {DM_Synth()}!2;
	{machines.do(_.makeInterface)}.defer;
	// m = machine;
}.fork
)
// for reverse compatibility reasons:
// m.oldload("/home/perso/DreamMachine/drums.dream-machine");