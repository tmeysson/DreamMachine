(
{
	var machine;
	Server.default.bootSync;
	DM_Synth.addDef;
	Server.default.sync;
	machine = DM_Synth();
	machine.makeMIDIInterface;
}.fork
)
