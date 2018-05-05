class TieNotation {
	constructor() {
		this.Syntax = {
			default: [
				FSM.item('Tie', /^\^/)
				// syntax 中直接包含状态机单元，会返回一个 Type: Tie
				// 将在遭遇时设置一个 Meta.After.Tie = true
				// 所有这些 Token 将统一在 epilog 中被删除
			]
		}
		this.Include = true
		// 是否加入命名空间豪华午餐，默认为 true
	}
	proMerge(result1, result2) {
		// 当执行音轨整合前调用，包括跨乐章的整合
		if (result1.Meta.After.Tie) {
			if ( /*有音符可以相接*/ ) {
				// 处理音符
			}
		}
	},
	epiNote() {
		// 当解析完音符后调用
		// this 是一个 TmNote 对象
		const PrevNote = []
		if (this.Meta.After.Tie) {
			// 对音符和前音符进行处理
		}
		this.Meta.User.PrevNote = PrevNote.concat(this.result)
		// After 和 User 属性都被默认设置为 Readable, Writable
	}
}