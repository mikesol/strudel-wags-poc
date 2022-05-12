import { mini as m } from "@strudel.cycles/mini";

export const mini = m;
export const mkCycle = (s) => {
	try {
		return m(s);
	} catch (e) {
		console.error(e);
		return m(" ");
	}
};
export const queryArc = (cycle) => (begin) => (end) =>
	cycle.queryArc(begin, end);
