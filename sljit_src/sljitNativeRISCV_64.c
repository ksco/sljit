/*
 *    Stack-less Just-In-Time compiler
 *
 *    Copyright Zoltan Herczeg (hzmester@freemail.hu). All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are
 * permitted provided that the following conditions are met:
 *
 *   1. Redistributions of source code must retain the above copyright notice, this list of
 *      conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright notice, this list
 *      of conditions and the following disclaimer in the documentation and/or other materials
 *      provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER(S) AND CONTRIBUTORS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
 * SHALL THE COPYRIGHT HOLDER(S) OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

static int trailing_zeros_64(sljit_uw x)
{
    // See http://supertech.csail.mit.edu/papers/debruijn.pdf
    static const sljit_u8 debruijn64tab[64] = {
        0,  1,  56, 2,  57, 49, 28, 3,  61, 58, 42, 50, 38, 29, 17, 4,
        62, 47, 59, 36, 45, 43, 51, 22, 53, 39, 33, 30, 24, 18, 12, 5,
        63, 55, 48, 27, 60, 41, 37, 16, 46, 35, 44, 21, 52, 32, 23, 11,
        54, 26, 40, 15, 34, 20, 31, 10, 25, 14, 19, 9,  13, 8,  7,  6,
    };

    static const sljit_uw debruijn64 = 0x03f79d71b4ca8b09ULL;
    if (x == 0) {
        return 64;
    }

    return (int)debruijn64tab[(x & -x) * debruijn64 >> (64 - 6)];
}

static sljit_s32 load_immediate_32(struct sljit_compiler *compiler, sljit_s32 dst_r, sljit_sw imm)
{
	/* Add 0x800 to cancel out the signed extension of ADDIW. */
    sljit_s32 hi20 = (imm + 0x800) >> 12 & 0xfffff;
    sljit_s32 lo12 = imm & 0xfff;
	sljit_s32 src_r = 0;
	if (hi20 != 0) {
		FAIL_IF(push_inst(compiler, LUI | RD(dst_r) | (sljit_ins)hi20));
	}
	if (lo12 != 0 || hi20 == 0) {
		FAIL_IF(push_inst(compiler, ADDIW | RD(dst_r) | RS1(src_r) | IMM_I(lo12));
	}
	return SLJIT_SUCCESS;
}

static sljit_s32 load_immediate(struct sljit_compiler *compiler, sljit_s32 dst_r, sljit_sw imm)
{
	if (((imm << 32) >> 32) == imm) {
		return load_immediate_32(compiler, dst_r, imm);
	}

	sljit_uw lo12 = (val << 52) >> 52;
	/* Add 0x800 to cancel out the signed extension of ADDI. */
	sljit_uw hi52 = (val + 0x800) >> 12;
	sljit_s32 shift = 12 + trailing_zeros_64((uint64)hi52);
	hi52 = ((hi52 >> (shift - 12)) << shift) >> shift;
	load_immediate(compiler, dst_r, imm);
	FAIL_IF(push_inst(compiler, SLLI | RD(dst_r) | RS1(dst_r) | IMM_I(shift)));
	if (lo12) {
		FAIL_IF(push_inst(compiler, ADDI | RD(dst_r) | RS1(dst_r) | IMM_I(lo12)));
	}
	return SLJIT_SUCCESS;
}

SLJIT_API_FUNC_ATTRIBUTE sljit_s32 sljit_emit_fset64(struct sljit_compiler *compiler,
	sljit_s32 freg, sljit_f64 value)
{
	union {
		sljit_sw imm;
		sljit_f64 value;
	} u;

	CHECK_ERROR();
	CHECK(check_sljit_emit_fset64(compiler, freg, value));

	u.value = value;

	if (u.imm == 0)
		return push_inst(compiler, FMV_W_X | (1 << 25) | RS1(TMP_ZERO) | FRD(freg));

	FAIL_IF(load_immediate(compiler, TMP_REG1, u.imm, TMP_REG3));
	return push_inst(compiler, FMV_W_X | (1 << 25) | RS1(TMP_REG1) | FRD(freg));
}

SLJIT_API_FUNC_ATTRIBUTE sljit_s32 sljit_emit_fcopy(struct sljit_compiler *compiler, sljit_s32 op,
	sljit_s32 freg, sljit_s32 reg)
{
	sljit_ins inst;

	CHECK_ERROR();
	CHECK(check_sljit_emit_fcopy(compiler, op, freg, reg));

	if (GET_OPCODE(op) == SLJIT_COPY_TO_F64)
		inst = FMV_W_X | RS1(reg) | FRD(freg);
	else
		inst = FMV_X_W | FRS1(freg) | RD(reg);

	if (!(op & SLJIT_32))
		inst |= (sljit_ins)1 << 25;

	return push_inst(compiler, inst);
}

static SLJIT_INLINE sljit_s32 emit_const(struct sljit_compiler *compiler, sljit_s32 dst, sljit_sw init_value, sljit_ins last_ins)
{
	sljit_sw high;

	if ((init_value & 0x800) != 0)
		init_value += 0x1000;

	high = init_value >> 32;

	if ((init_value & 0x80000000l) != 0)
		high = ~high;

	if ((high & 0x800) != 0)
		high += 0x1000;

	FAIL_IF(push_inst(compiler, LUI | RD(TMP_REG3) | (sljit_ins)(high & ~0xfff)));
	FAIL_IF(push_inst(compiler, ADDI | RD(TMP_REG3) | RS1(TMP_REG3) | IMM_I(high)));
	FAIL_IF(push_inst(compiler, LUI | RD(dst) | (sljit_ins)(init_value & ~0xfff)));
	FAIL_IF(push_inst(compiler, SLLI | RD(TMP_REG3) | RS1(TMP_REG3) | IMM_I(32)));
	FAIL_IF(push_inst(compiler, XOR | RD(dst) | RS1(dst) | RS2(TMP_REG3)));
	return push_inst(compiler, last_ins | RS1(dst) | IMM_I(init_value));
}

SLJIT_API_FUNC_ATTRIBUTE void sljit_set_jump_addr(sljit_uw addr, sljit_uw new_target, sljit_sw executable_offset)
{
	sljit_ins *inst = (sljit_ins*)addr;
	sljit_sw high;
	SLJIT_UNUSED_ARG(executable_offset);

	if ((new_target & 0x800) != 0)
		new_target += 0x1000;

	high = (sljit_sw)new_target >> 32;

	if ((new_target & 0x80000000l) != 0)
		high = ~high;

	if ((high & 0x800) != 0)
		high += 0x1000;

	SLJIT_UPDATE_WX_FLAGS(inst, inst + 5, 0);

	SLJIT_ASSERT((inst[0] & 0x7f) == LUI);
	inst[0] = (inst[0] & 0xfff) | (sljit_ins)(high & ~0xfff);
	SLJIT_ASSERT((inst[1] & 0x707f) == ADDI);
	inst[1] = (inst[1] & 0xfffff) | IMM_I(high);
	SLJIT_ASSERT((inst[2] & 0x7f) == LUI);
	inst[2] = (inst[2] & 0xfff) | (sljit_ins)((sljit_sw)new_target & ~0xfff);
	SLJIT_ASSERT((inst[5] & 0x707f) == ADDI || (inst[5] & 0x707f) == JALR);
	inst[5] = (inst[5] & 0xfffff) | IMM_I(new_target);
	SLJIT_UPDATE_WX_FLAGS(inst, inst + 5, 1);

	inst = (sljit_ins *)SLJIT_ADD_EXEC_OFFSET(inst, executable_offset);
	SLJIT_CACHE_FLUSH(inst, inst + 5);
}
